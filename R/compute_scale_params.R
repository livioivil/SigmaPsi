#' A series of preformed setting for computing scales of a few tests
#' 
#' @title Setting for computing scales of a few tests
#' @name scale_params
#' @aliases scale_dass_params scale_ecr_params scale_das_params scale_scl_params scale_biaq_params scale_tas20_params scale_tas24_params
#' @description 
#' DAS
#'item dal 1 al 15, 18 e 19: i valori degli item vanno inseriti da 5 (tot d’accordo, sempre) a 0 (tot disaccordo, mai)
#'item dal 16, 17, 20 al 22, dal 25 al 28: i valori degli item vanno inseriti da 0 (sempre, mai) a 5 (mai, sempre) 
#'item 23, 24: da 4 a 0
#'item 29, 30: 0 – 1
#'item 31: 0 (estremamente infelice) a 6 (perfetto)
#'item 32: a=5, b=4, c=3, d=2, e=1, f=0
#'
#'calcola F1. dyadic consensus: somma item: 1+2+3+5+7+8+9+10+11+12+13+14+15
#'calcola F2. dyadic satisfaction: somma item: 16+17++18+19+20+21+22+23+31+32
#'calcola F3: affectional expression: somma item: 4+6+29+30
#'calcola F4: diadi cohesion: somma item: 24+25+26+27+28
#'totale DAS = f1+f2+f3+f4
#'
#'valori normativi in punti T (media 50, ds 10 - norma 40-60), separati per sposati e divorziati.
#'
#'
#'ECR 
#'(36 item, da 7 a 1 - massimo 252 –tot disaccordo-, minimo 36 – totale accordo-)
#'girare item (mettergli r): 3, 15, 19, 22,25, 27, 29, 31, 33, 35.
#'item dispari = evitamento (18 item: min 18 relaz ok, max 126 relaz ko)
#'item pari = ansietà (18 item: min 18 ok, max 126 relaz ko)
#'misura qualitativa di profilo, per soggetto. Non ci sono cut off. La media per scala è 63, ds 12,6
#'trasforma in punti T: 50:63=10:12,6
#'cut off sensato: 
#'  norma 50-76
#'76 a 82 border problematico
#'>82 patologico evitamento o ansietà
#'
#'
#'   fattori TAS-20 e globale 
#'   fattori TAS-24 e globale
#'   f1 difficoltà identificare sentimenti
#'   f2 difficoltà comprendere sentimenti
#'   f3 pensiero orient esternamente
#'   cut off globale: norma punteggi inferiori a 55 ? soggetti non alessitimici; 
#'   punteggi compresi tra 55 e 64 ? soggetti borderline; 
#'   punteggi superiori a 64   ? soggetti alessitimici.
#'   
#' @export



scale_ecr_params <- list(invert=c(34,8, 36, 23, 5, 32, 3, 33, 25,16,21,15,19,24),
                          values.ranges=c(1,7),
                          na.action=NA,
                          transf=function(items){
                            id_ansia=c(13,31,7,22,26,11,30,35,19,27,24,9,14,28,29,6,1,18)
                            id_evita=setdiff(1:36,id_ansia)
                            cbind(ECR.avoidance=rowMeans(items[,id_evita],na.rm=TRUE),
                                  ECR.anxiety=rowMeans(items[,id_ansia],na.rm=TRUE))
                          }
)

#' @export
scale_das_params  <-list(invert=c(1:15, 18, 19,23, 24,32),
                         values.ranges=cbind(matrix(c(0,5),2,22),
                                             c(0,4),c(0,4),
                                             c(0,5),c(0,5),c(0,5),c(0,5),
                                             c(0,1),c(0,1),c(0,6),c(0,5)),
                         na.action=NA,
                         transf=function(items){
                           ids=list(F1=c(1,2,3,5,7,8,9,10,11,12,13,14,15),
                                    F2=c(16,17,18,19,20,21,22,23,31,32),
                                    F3=c(4,6,29,30),
                                    F4=c(24,25,26,27,28),
                                    Tot=1:32)
                           rowSums.bylist(items,ids)
                         })
#' @export
scale_tas20_params  <-list(  invert=c(4,5,10,18,19,20),
                             values.ranges=c(1,5),
                             na.action=NA,
                             transf=function(items){
                               ids=list(
                                 f1_difidentsent =c( 1,3,6,7,9,13,14),
                                 f2_difcomprsent =c( 2,4,11,12,17),
                                 f3_pensester =c(5,8,10,15,16,18,19, 20),
                                 tot =1:20)
                               rowMeans.bylist(items,ids)
                         })

#' @export
scale_tas24_params  <-list(  invert=c(3 ,5 ,7 ,11 ,12 ,13 ,19 ,23),
                             values.ranges=c(1,5),
                             na.action=NA,
                             transf=function(items){
                               ids=list(f1 =c( 1,4,6,7,13,17,22),
                                      f2 =c( 8,10,16,18,20,24),
                                      f3 =c( 5,11,12,14,15,19, 23),
                                      f4 =c( 2,3,9,21),
                                      tot =1:24)
                               rowMeans.bylist(items,ids)
                               })


#' @export
scale_scl_params  <-list(invert=c(),
                         values.ranges=NULL,
                         na.action=NA,
                         transf=function(items){
                           ids=list(sen=c(6,21,34,36,37,41,61,69,73),
                                    som=c(1,4,12,27,40,42,48,49,52,53,56,58),
                                    oss=c(3,9,10,28,38,45,46,51,55,65),
                                    depr=c(5,14,15,20,22,26,29,30,31,32,54,71,79),
                                    ansi=c(2,17,23,33,39,57,72,78,80,86),
                                    col=c(11,24,63,67,74,81),
                                    ansf=c(13,25,47,50,70,75,82),
                                    idpar=c(8,18,43,68,76,83),
                                    psi=c(7,16,35,62,77,84,85,87,88,90),
                                    gsi=1:90)
                           rowMeans.bylist(items,ids)
                         })

#' @export
scale_biaq_params  <-list(invert=c(),
                          values.ranges=NULL,
                          na.action=NA,
                          transf=function(items){
                            ids=list(
                              clothin=c(1,2,3,4,13,15,16,17,18),
                              socialactivities=c(8,9,10,11),
                              eatingrestrant=c(5,6,7),
                              groomingwheighing =c(12,14,19),
                              tot=1:19)
                            rowMeans.bylist(items,ids)
                          })

#' @export 
scale_dass_params <- list(values.ranges=c(0,3),
                          na.action=NA,
                          transf=function(items){
                            id_depression=c(3,5,10,13,16,17,21)
                            id_anxiety=c(2,4,7,9,15,19,20)
                            id_stress=c(1,6,8,11,12,14,18) #setdiff(1:21,c(id_depression,id_anxiety))
                            cbind(DASS.depression=rowSums(items[,id_depression,drop=FALSE],na.rm=TRUE),
                                  DASS.anxiety=rowSums(items[,id_anxiety,drop=FALSE],na.rm=TRUE),
                                  DASS.stress=rowSums(items[,id_stress,drop=FALSE],na.rm=TRUE))
                          }
)


#' @export 
scale_narcissistic_params <- list(values.ranges=c(0,13),
                                  na.action=NA,
                                  transf=function(items){
                                    items_narcissistic_ita=list(
                                      narc1=c("^Non mi","^Trovo"),
                                      narc2=c("^Quando le","^So che"),
                                      narc3=c("^Non mi", "^Mi piace "),
                                      narc4=c("^Di solito","^Insisto nel"),
                                      narc5=c("^Non mi","^Mi piace"),
                                      narc6=c("^Il potere","^Ho una "), 
                                      narc7=c("^Mi piace","^Mi aspetto"),
                                      narc8=c("^Il mio ","^Mi piace"),
                                      narc9=c("^Comandare non","^Le persone"),
                                      narc10=c("^Prender","^Non sono"),
                                      narc11=c("^Cerco di","^Di solito"),
                                      narc12=c("^La capacit","^Sono un"), 
                                      narc13=c("^Non sono","^Mi piace")
                                    )
                                    
                                    items0=items
                                    items=array(NA,dim(items0))
                                    for (i in 1:ncol(items)){
                                      items[grep(items_narcissistic_ita[[i]][1],items0[,i]),i]=0                                      
                                      items[grep(items_narcissistic_ita[[i]][2],items0[,i]),i]=1
                                    }
                                    
                                    
                                    id_leadership=c(3, 6, 9, 12)
                                    id_esibizionismo=c(2, 5, 8, 11,13)
                                    id_sfruttamento=c(1, 4, 7, 10) 
                                    
                                    cbind(NARCIS.leadership=rowSums(items[,id_leadership,drop=FALSE],na.rm=TRUE),
                                          NARCIS.esibiz=rowSums(items[,id_esibizionismo,drop=FALSE],na.rm=TRUE),
                                          NARCIS.sfrutt=rowSums(items[,id_sfruttamento,drop=FALSE],na.rm=TRUE),
                                          NARCIS.totale=rowSums(items,na.rm=TRUE))
                                  }
)
