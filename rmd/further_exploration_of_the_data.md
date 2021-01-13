Further Exploration of the data
================
Arpan Neupane
26/06/2020

\#\#load libraries

``` r
library(ggplot2)
library(tidyverse)
library(tidytext)
library(ggiraph)
```

## Cluster by authors.

Would it be possible to cluster research articles by authors? There must
be \>100K authors in the dataset. Let’s limit to top-20
authors

``` r
#reading data in from previous filtering step (look at file titled generating initial plots)
path <- paste(getwd(),"datafiles/prelim_data",sep = "/")
path <- path %>% gsub("rmd/","",.)
my_data <- readRDS(file = path)
t_20 <- my_data %>% unnest(authorlist) %>% na.omit %>% filter(authorlist != "NA NA") %>% group_by(authorlist,dataset) %>% 
  summarize(total_pub = n()) %>% arrange(desc(total_pub)) %>% head(20) %>% select(authorlist) %>% pull;t_20
```

    ## `summarise()` regrouping output by 'authorlist' (override with `.groups` argument)

    ##  [1] "Darragh Duffy"      "Wei Wang"           "Florent Ginhoux"   
    ##  [4] "Wei Wang"           "Guido Kroemer"      "Wei Li"            
    ##  [7] "Wei Zhang"          "Alberto Briganti"   "Xin Li"            
    ## [10] "Yi Zhang"           "Li Zhang"           "Shahrokh F Shariat"
    ## [13] "Wei Chen"           "Yang Liu"           "David Sancho"      
    ## [16] "Jing Wang"          "Jing Wang"          "Li Chen"           
    ## [19] "Tanja D de Gruijl"  "Vincent Bondet"

``` r
#note that the t_20 is also based on dataset grouping. 
```

Now that we have a list of top 20 authors. We will filter the dataset to
rows only if they contain some of these authors, and get the PMIDs of
the filtered
data

``` r
t_20_pmid <- my_data %>% unnest(authorlist) %>% na.omit %>% filter(authorlist!= "NA NA") %>% filter(authorlist %in% t_20) %>% select(PMID) %>% unique %>% pull
length(t_20_pmid)
```

    ## [1] 492

``` r
my_data %>% filter(PMID %in% t_20_pmid) %>% select(abstract) %>% unique %>% pull %>% length
```

    ## [1] 379

``` r
#some of these data have missing abstract. Either way; lets get them out.

my_df_small <- my_data %>% filter(PMID %in% t_20_pmid) %>% select(PMID, authorlist, country) %>% unnest(authorlist)

#how many unique authors?
my_df_small %>% select(authorlist) %>% unique %>% pull %>% length
```

    ## [1] 4643

The small dataset, initially filtered by top 20 authors, now has 4643
(including all sub authors etc)

Will now count occurrences of each author, by PMID, and look at
correlation between different PMIDs. Note that we have \~492 unique
PMIDs

First, lets look through who is already working together. For this case,
I will pull out authorlists for people working with Darragh Duffy and
Florent
Ginhoux

``` r
florent_authors <- my_data %>% rowwise %>% mutate(authorlist = unlist(authorlist) %>% paste(collapse=",")) %>% mutate(to_keep = grepl("Florent Ginhoux",authorlist)) %>% 
  filter(to_keep) %>% select(authorlist) %>% pull %>% paste(collapse = ",") %>% str_split(pattern = ",") %>% unlist %>% unique;length(florent_authors)
```

    ## [1] 713

``` r
florent_pmid <- my_data %>% rowwise %>% mutate(authorlist = unlist(authorlist) %>% paste(collapse=",")) %>% mutate(to_keep = grepl("Florent Ginhoux",authorlist)) %>% 
  filter(to_keep) %>% select(PMID) %>% pull 

length(florent_pmid)
```

    ## [1] 24

There are 713 unique authors that florent ginhoux has “collaborated”
with in 24 unique PMIDs

Can we find a similar structure, but graphically? Remember to filter
top\_20 pmid

``` r
#library(tidytext)

my_data_tokens <- my_data %>% filter(PMID %in% t_20_pmid) %>% na.omit %>% 
  rowwise() %>% mutate(authorlist = unlist(authorlist) %>% paste(collapse=",") %>% gsub(" ","",x = .) %>% gsub("-","",x=.)) %>% 
  unnest_tokens(authors,authorlist) %>% group_by(PMID,authors) %>% summarize(counts = n())
```

    ## `summarise()` regrouping output by 'PMID' (override with `.groups` argument)

``` r
my_data_tokens <- my_data_tokens %>% arrange(desc(counts)) %>% mutate(counts = ifelse(counts>=2,1,counts))#noticed the PMID had authors name repeated for some reason.

my_data_tokens_spread <- my_data_tokens %>% spread(key=authors, value = counts) %>% replace(is.na(.),0)

my_data_tokens_matrix <- as.matrix(my_data_tokens_spread[,c(2:(ncol(my_data_tokens_spread)-1))])
rownames(my_data_tokens_matrix) <- my_data_tokens_spread$PMID
#view small bit
my_data_tokens_matrix[1:2,1:2]
```

    ##          aaronbossler aaronjmarshall
    ## 30694364            0              0
    ## 30709742            0              0

Now that we have created a matrix. Can we find correlation between
authors? Alternatively, can we find correlation between PMID ? Not sure
if this is correct way of doing this.

``` r
dist_pmid <- dist(my_data_tokens_matrix,method = "binary")
mds <- cmdscale(d = dist_pmid,eig = F,k = 2)
#check if rownames PMID were kept
rownames(mds) %>% head
```

    ## [1] "30694364" "30709742" "30738087" "30772194" "30774630" "30778250"

``` r
#combine to a tibble and plot

tibble(x=mds[,1],y=mds[,2],PMID = rownames(mds)) %>% left_join(my_data,by="PMID") %>% 
  ggplot(aes(x = x, y = y, fill = dataset))+
  geom_point(aes(color = dataset))
```

![](further_exploration_of_the_data_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
# make intermediate dataframe; of only top_20_pmids 
# also include search terms
search_terms <- c("b cells|b-cells|b cell|b-cell","t-cells|t cells|t cell|t-cell","dendritic|dc|dcs|dendritic cells","macrophages|macrophage",
                  "neutrophil|neutrophils","lymph node|LN|lymph nodes")

top_20_inter <- my_data %>% filter(PMID %in% t_20_pmid) %>% na.omit %>% 
  rowwise() %>% mutate(authorlist = unlist(authorlist) %>% paste(collapse=",")) %>% 
  mutate(articletitle = gsub("'","",x = articletitle)) %>% mutate(combine_both = paste0(abstract,articletitle)) %>% mutate(title_contains= case_when(
  grepl(search_terms[1],combine_both,ignore.case = T)~"B Cell related",
  grepl(search_terms[2],combine_both, ignore.case = T)~"T Cell related",
  grepl(search_terms[3],combine_both, ignore.case = T)~"Dendritic Cell related",
  grepl(search_terms[4],combine_both, ignore.case = T)~"Macrophage related",
  grepl(search_terms[5],combine_both, ignore.case = T)~"Neutrophil related",
  grepl(search_terms[6],combine_both, ignore.case = T)~"Lymph Node related")
  ) %>% select(-combine_both) %>% mutate(florent_true = ifelse(PMID %in% florent_pmid,TRUE,FALSE))
glimpse(top_20_inter)
```

    ## Rows: 492
    ## Columns: 10
    ## Rowwise: 
    ## $ PMID           <chr> "32554932", "32546725", "32502047", "32481780", "324...
    ## $ articletitle   <chr> "Single cell rna sequencing identifies an early mono...
    ## $ abstract       <chr> "The acute respiratory distress syndrome (ARDS) resu...
    ## $ authorlist     <chr> "Yale Jiang,Brian R Rosborough,Jie Chen,Sudipta Das,...
    ## $ journal        <chr> "JCI insight", "Leukemia", "Medicine", "Rhode Island...
    ## $ publishedon    <date> 2020-06-20, 2020-06-18, 2020-06-06, 2020-06-03, 202...
    ## $ country        <chr> "United States", "England", "United States", "United...
    ## $ dataset        <chr> "neutrophil", "neutrophil", "neutrophil", "neutrophi...
    ## $ title_contains <chr> "Neutrophil related", NA, "Neutrophil related", "Neu...
    ## $ florent_true   <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FAL...

``` r
# label florent ginhoux papers
library(ggrepel)
```

    ## Warning: package 'ggrepel' was built under R version 3.6.3

``` r
plot_mds <- 
  tibble(x=mds[,1],y=mds[,2],PMID = rownames(mds)) %>% left_join(top_20_inter,by="PMID") %>% 
  ggplot(aes(x = x, y = y, color = as.factor(florent_true), alpha = florent_true))+
  geom_point()+
  geom_text_repel(aes(label = ifelse(florent_true,as.character(florent_true),"")))+
  theme_minimal()+
  scale_color_discrete(aesthetics = c("color"), name = "PMID\nFlorent Ginhoux",labels = c("False","True"),)+
  scale_alpha_discrete(guide=F)+
  labs(title = "MDS scaling plot",subtitle = "Clustering by authors",caption = "Top 20 authors were identified, observations filtered by presence of\none of these others.\n
       Authors were tokenized and distance calculated.")
```

    ## Warning: Using alpha for a discrete variable is not advised.

``` r
plot_mds
```

![](further_exploration_of_the_data_files/figure-gfm/unnamed-chunk-6-2.png)<!-- -->

``` r
path <- gsub("prelim_data","cluster_by_author.pdf",path)
ggsave(path,plot = plot_mds, device = "pdf",dpi = 300)
```

    ## Saving 7 x 5 in image

The plot shows that it is possilbe to find colloborators (by direct
inclusion of names in author list) or by closeness. This may be not be
the best approach of clustering by authors. However, I think it could be a
step in the right direction.

\#Can we further improve this? What if we treated each document as a
results from single cell RNAseq experiment. I do not have expertise in
this. However, since we are counting “features” per document and looking
across all our documents, I thought why not?

The following was adaptedfrom Satijalab.org tutorial on singlecell
clustering with Seurat

``` r
#remove PMID with no abstract
my_data <- my_data %>% ungroup %>%  filter(abstract != "")

#combine title, author, abstract into a single feature
my_data_concat <- my_data %>% filter(country!="China") %>% rowwise() %>% mutate(authorlist = unlist(authorlist) %>% paste(collapse=",") %>% gsub(" ","",x=.)) %>% filter(authorlist != "NANA") %>% 
mutate(combined = paste(articletitle,abstract,authorlist,sep=","))

#remove puncuations
my_data_concat <- my_data_concat %>% rowwise %>% mutate(combined = gsub("-|'|:|=|<|>|(|)","",x=combined))

glimpse(my_data_concat)

my_data_start <- my_data_concat %>% select(PMID,combined)

#make sure tidytext is loaded
library(tidytext)
#tokenize words
my_data_start_all <- my_data_start %>% group_by(PMID) %>% unnest_tokens(output = word,combined,to_lower = T) #%>% 

#adding some immunology related words to stop_words
tb_word <- tibble(word = c(stop_words$word,"conclusion","conclusions","result","results","background","methods",
                           "method","material","materials","keyword","keywords","main","hypothesis","immune","cells","cell","immunology","immunity",
                           "mean","se","sd","cv","ci","pvalue","p","0.05","0.01","0.95","95","control","tx","con","nature","confidenceinterval",
                           "confidence","interval","significant","0.001","objective","0.001","inflammation","findings","controls","suggest","suggests",
                           "levels","level","level","difference","significance","abstracttext","analysis","analyze","treated",
                           as.character(c(1:10)),"significantly"))

#remove stop_words
my_data_start_all <- my_data_start_all %>%ungroup %>% anti_join(tb_word,by = "word")

#did this to generate tf, idf, and tf_idf for all
my_data_start_all <- my_data_start_all %>% ungroup %>% rowwise %>%  mutate(char_length = nchar(word)) %>% filter(char_length > 2) %>% select(-char_length)

my_data_start_all <- my_data_start_all %>% group_by(PMID,word) %>% summarize(count = n()) %>% ungroup %>% bind_tf_idf(word,PMID,count)

#cast as a sparse matrix
my_data_tfidf <- cast_sparse(my_data_start_all,row = PMID,column = word,value = count)
my_data_tfidf_t <- Matrix::t(my_data_tfidf)
rm(my_data_tfidf)
#write some metadeta
meta_data <- my_data_concat %>% 
  select(-c(abstract,publishedon,journal,country,combined)) %>% rowwise %>% mutate(articletitle = gsub("'","",x = articletitle)) %>% 
  as.data.frame
rownames(meta_data) <- meta_data$PMID
meta_data <- meta_data %>% select(-PMID)
meta_data %>% head

if(!"Seurat" %in% installed.packages()){
  install.packages("Seurat")
}else(library(Seurat))

#i think the min cells is to select only cells with at least 10 features 
#min.features is to keep features that are present in atleast 5 cells
doc_seurat <- CreateSeuratObject(counts = my_data_tfidf_t, meta.data = meta_data,min.cells = 5,min.features = 10)
doc_seurat
#features were reduced to ~20K

#"Normalizing". Need to read more about what this is doing
doc_seurat <- NormalizeData(doc_seurat, normalization.method = "LogNormalize", scale.factor = 10000)

#I think this is findining the features that have the highest variation
doc_seurat <- FindVariableFeatures(doc_seurat)
doc_seurat
#suggesting that 2000 features are interesting (highly variable), other are "similar"?
doc_seurat <- ScaleData(doc_seurat)


#this does PCA with highly variable features or all features? not sure. 
doc_seurat <- RunPCA(doc_seurat, npcs = 100, ndims.print = 1:10, nfeatures.print = 5)
#i think the above code will only generate eigen vectors for the first npcs principle component (?)
#is much faster that running prcomp or princomp
#setting npcs higher makes it run longer

#show sd by PCs?
ElbowPlot(doc_seurat, ndims = 100)
#most variation in the initial pcs then start tapering off (slowly)

#the tutorial used 75 pcs, following that
#most other parameters are left alone. need to read what they are and what they do
doc_seurat <- FindNeighbors(doc_seurat, reduction = "pca", dims = 1:75, nn.eps = 0.5)
doc_seurat <- FindClusters(doc_seurat, resolution = 3, n.start = 10)#took ~3-5 minutes

#dimension reduction
doc_seurat <- RunUMAP(doc_seurat, dims = 1:75, min.dist = 0.75,)
```

Visualizing

``` r
d1 <- DimPlot(doc_seurat, reduction = "umap", pt.size = 0.1,group.by = "dataset") + ggtitle(label = "UMAP\ncolored by initial search query")
d2 <- DimPlot(doc_seurat, reduction = "umap", pt.size = 0.1,label = T)+ggtitle(label = "UMAP\ncolored by clusters assiged by Seurat")+theme(legend.position ="none")
```

    ## Warning: Using `as.character()` on a quosure is deprecated as of rlang 0.3.0.
    ## Please use `as_label()` or `as_name()` instead.
    ## This warning is displayed once per session.

``` r
d1
```

![](further_exploration_of_the_data_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
d2
```

![](further_exploration_of_the_data_files/figure-gfm/unnamed-chunk-9-2.png)<!-- -->

Ok so this looks like an interesting approach. First, the graphs look
nicer\! there are actual “clusters” compared to just doing pca.

HoverLocator (also from
Seurat)

``` r
#HoverLocator(plot = d1, information =FetchData(doc_seurat,vars = "articletitle"))
```

What if we want to search for specific terms?

``` r
plot_what <- function(whattolookfor){
  #what to look for can be a single string or a vector of characters
  #remove space, not that not all search terms will yeild results
  FeaturePlot(doc_seurat,features = whattolookfor, reduction = "umap",pt.size = 0.1)
}

#this shows some issues that need to be dealt with.
plot_what(c("neutrophil"))
```

![](further_exploration_of_the_data_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

Issues still exist. For example: neutrophil vs neutrophils might not
results in same documents being identified. See
below:

``` r
plot_what(c("neutrophils","neutrophil"))
```

![](further_exploration_of_the_data_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->
However, we did get some clustering going based on features.

I also tried to use the FindMarkers function to identify what terms were
driving clustering.

``` r
feature_information <- FindAllMarkers(doc_seurat)
```

Now if only I could figure out how to label clusters according to the
terms that were higher in them\! Maybe find a “general”
    theme?

``` r
feature_information %>% head
```

    ##                      p_val  avg_logFC pct.1 pct.2    p_val_adj cluster
    ## patients      1.076003e-55 -0.5557868 0.253 0.445 2.321799e-51       0
    ## lymph         8.305986e-38 -0.5915755 0.095 0.222 1.792266e-33       0
    ## node          1.857268e-29 -0.6794664 0.065 0.161 4.007614e-25       0
    ## significantly 3.682722e-27 -0.5904330 0.125 0.233 7.946578e-23       0
    ## cancer        1.166286e-26 -0.4728664 0.165 0.277 2.516611e-22       0
    ## survival      1.079557e-20 -0.5845156 0.093 0.176 2.329467e-16       0
    ##                        gene
    ## patients           patients
    ## lymph                 lymph
    ## node                   node
    ## significantly significantly
    ## cancer               cancer
    ## survival           survival

``` r
feature_information %>% group_by(cluster) %>% filter(p_val_adj<0.05) %>% summarise(features = paste(gene, sep=",")[1:5] %>% paste(.,collapse="," ))
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## # A tibble: 102 x 2
    ##    cluster features                                      
    ##    <fct>   <chr>                                         
    ##  1 0       patients,lymph,node,significantly,cancer      
    ##  2 1       aki,kidney,ngal,urinary,renal                 
    ##  3 2       axillary,breast,alnd,her2,mastectomy          
    ##  4 3       treg,tregs,th17,foxp3,regulatory              
    ##  5 4       allergic,asthma,airway,ige,rhinitis           
    ##  6 5       pd1,pdl1,programmed,checkpoint,ctla4          
    ##  7 6       cd80,dcs,dendritic,cd86,cd40                  
    ##  8 7       lymphoma,dlbcl,lymphomas,diffuse,bcell        
    ##  9 8       igm,igg,iga,immunoglobulin,antibodies         
    ## 10 9       pregnancy,maternal,placental,fetal,endometrial
    ## # ... with 92 more rows

Visualizing this information

``` r
d1 <- DimPlot(doc_seurat,reduction = "umap")
umap_data <- d1$data
umap_data <- umap_data %>% mutate(PMID = rownames(umap_data))
umap_data <- umap_data %>% right_join(feature_information %>% mutate(ident = cluster) %>% select(-cluster) %>%
                                        filter(p_val_adj<0.05) %>% group_by(ident) %>% 
                                        summarise(features = paste(gene,sep=",")[1:2] %>% paste(.,collapse = ", ")),
                                      by = "ident") %>% as_tibble()
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
my_data <- my_data %>% ungroup %>% filter(PMID %in% umap_data$PMID)
                            
umap_data <- umap_data %>% right_join(my_data ,by = "PMID")
umap_data.mean <- umap_data %>% group_by(ident,features) %>% summarise(m.UMAP_1 = mean(UMAP_1),m.UMAP_2 = mean(UMAP_2))
```

    ## `summarise()` regrouping output by 'ident' (override with `.groups` argument)

``` r
library(ggrepel)
g <- umap_data %>% ggplot(aes(x = UMAP_1, y = UMAP_2,color = dataset), show.legend=F)+
  geom_point(size = 0.01)+
  geom_point(data = umap_data.mean,aes(x = m.UMAP_1, y = m.UMAP_2), color = "black", size = 2)+
  geom_text_repel(data = umap_data.mean, aes(x = m.UMAP_1, y = m.UMAP_2, label = features),inherit.aes = F)+
  theme(legend.position = "none")
g
```

![Only top 2 terms are
shown](further_exploration_of_the_data_files/figure-gfm/unnamed-chunk-15-1.png)
