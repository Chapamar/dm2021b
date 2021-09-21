#Feature Engineering
#creo nuevas variables dentro del mismo mes
#Condimentar a gusto con nuevas variables

#limpio la memoria
rm( list=ls() )
gc()

require("data.table")

#setwd("~/buckets/b1/")
setwd("C:/Users/Martin/Desktop/Carrera esp Datos_ITBA/Data Mining_E&F")
#cargo el dataset donde voy a entrenar
dataset  <- fread("./datasetsOri/paquete_premium_202011.csv")

#Establezco el Working Directory
#setwd( "~/buckets/b1/crudo" )
##Analisis exploratorio###
table(dataset$Visa_status)
summary(dataset$cproductos)
summary(dataset$mcuentas_saldo)
table(dataset$tcuentas)
summary(dataset$ccajas_transacciones)
summary(dataset$cliente_edad)
summary(dataset$ccajas_extracciones)
table(dataset$Visa_delinquency)
summary(dataset$Visa_mlimitecompra)
summary(dataset$Master_mlimitecompra)
summary(dataset$cliente_antiguedad)
table(dataset$Visa_status)

EnriquecerDataset <- function( dataset , arch_destino )
{
  columnas_originales <-  copy(colnames( dataset ))
  
  #INICIO de la seccion donde se deben hacer cambios con variables nuevas
  #discretizar y combinar las variables mas relevantes
  dataset[ , mv_status            := ifelse (Master_status==0 |  Visa_status==0, 0 ,ifelse (Master_status!0 |  Visa_status!0, 1))]
  dataset[ , cprod_cat            := ifelse (cproductos>=8 , 0 ,ifelse (cproductos <8 , 1))]
  dataset[ , mcuentas_saldo_cat   := ifelse (mcuentas_saldo   >= 119486, 0 ,ifelse (cproductos < 119486, 1))]    
  dataset[ , cliente_edad_cat     := ifelse (cliente_edad  >= 55, 0 ,ifelse (cliente_edad < 55, 1))]
  dataset[ , mv_delinquency       := ifelse (Master_delinquency==0 |  Visa_delinquency==0, 1 , ifelse (Master_delinquency==1 |  Visa_delinquency==1, 0)) ]
  dataset[ , mv_mlimitecompra     := ifelse (Master_mlimitecompra<=503734 |  Visa_mlimitecompra<=503734, 0 ,ifelse (Master_mlimitecompra>503734 |  Visa_mlimitecompra>503734,1))]
  dataset[ , cliente_antiguedad_cat := ifelse (cliente_antiguedad  >= 185, 0 ,ifelse (cliente_antiguedad < 185, 1))]
  
  #combino MasterCard y Visa de manera loca
  dataset[ , mv_mfinanciacion_limite := rowSums( cbind( Master_mfinanciacion_limite,  Visa_mfinanciacion_limite) , na.rm=TRUE ) ]
  dataset[ , mv_msaldopesos          := rowSums( cbind( Master_msaldopesos,  Visa_msaldopesos) , na.rm=TRUE ) ]
  dataset[ , mv_msaldodolares        := rowSums( cbind( Master_msaldodolares,  Visa_msaldodolares) , na.rm=TRUE ) ]
  dataset[ , mv_mlimitecompra        := rowSums( cbind( Master_mlimitecompra,  Visa_mlimitecompra) , na.rm=TRUE ) ]
  dataset[ , mv_mconsumototal        := rowSums( cbind( Master_mconsumototal,  Visa_mconsumototal) , na.rm=TRUE ) ]
  dataset[ , mv_cconsumos            := rowSums( cbind( Master_cconsumos,  Visa_cconsumos) , na.rm=TRUE ) ]
  dataset[ , mv_mpagominimo          := rowSums( cbind( Master_mpagominimo,  Visa_mpagominimo) , na.rm=TRUE ) ]
  
  #ratio de variables conocidas y desconocidas
  dataset[ , r_prestamos_personales  := mprestamos_personales / cprestamos_personales, na.rm=TRUE ]
  dataset[ , r_prestamos_hipotecarios:= mprestamos_hipotecarios / cprestamos_hipotecarios, na.rm=TRUE ]
  dataset[ , r_inversion1_dolares    := minversion1_dolares/ cinversion1, na.rm=TRUE ]
  dataset[ , r_inversion1_pesos      := minversion1_pesos/ cinversion1, na.rm=TRUE ]
  dataset[ , r_inversion2            := minversion2/ cinversion2, na.rm=TRUE ]
  dataset[ , r_cajeros_propios_descuentos  := mcajeros_propios_descuentos/ ccajeros_propios_descuentos, na.rm=TRUE ]
  dataset[ , r_comisiones_mantenimiento := mcomisiones_mantenimiento/ ccomisiones_mantenimiento, na.rm=TRUE ]
  dataset[ , r_transferencias_recibidas := mtransferencias_recibidas/ ctransferencias_recibidas, na.rm=TRUE ]
  dataset[ , r_transferencias_emitidas := mtransferencias_emitidas/ ctransferencias_emitidas, na.rm=TRUE ]
  dataset[ , r_cheques_depositados := mcheques_depositados/ ccheques_depositados, na.rm=TRUE ]
  dataset[ , r_plazo_fijo := (mplazo_fijo_dolares+mplazo_fijo_pesos)/ cplazo_fijo, na.rm=TRUE ]
  
  dataset[ , mvr_msaldototal         := mv_msaldototal / mv_mlimitecompra, na.rm=TRUE ]
  dataset[ , mvr_msaldopesos2        := mv_msaldopesos / mv_msaldototal, na.rm=TRUE ]
  dataset[ , mvr_msaldodolares2      := mv_msaldodolares / mv_msaldototal, na.rm=TRUE ]
  dataset[ , mvr_mconsumospesos      := mv_mconsumospesos / mv_mlimitecompra,, na.rm=TRUE ]
  dataset[ , mvr_mconsumosdolares    := mv_mconsumosdolares / mv_mlimitecompra,, na.rm=TRUE ]
  dataset[ , mvr_madelantodolares    := mv_madelantodolares / mv_mlimitecompra, na.rm=TRUE ]
  