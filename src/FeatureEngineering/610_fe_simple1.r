#Feature Engineering
#creo nuevas variables dentro del mismo mes
#Condimentar a gusto con nuevas variables

#limpio la memoria
rm( list=ls() )
gc()

require("data.table")

#Establezco el Working Directory
#setwd("~/buckets/b1/")
setwd("C:/Users/Martin/Desktop/Carrera esp Datos_ITBA/Data Mining_E&F")
#cargo el dataset donde voy a entrenar
dataset  <- fread("./datasetsOri/paquete_premium_202011.csv")



EnriquecerDataset <- function( dataset , arch_destino )
{
  columnas_originales <-  copy(colnames( dataset ))

  #INICIO de la seccion donde se deben hacer cambios con variables nuevas
  #discretizar y combinar las variables mas relevantes
  dataset[ , mv_status            := ifelse (Master_status==0 |  Visa_status==0, 0 , 1)]
  dataset[ , cprod_cat            := ifelse (cproductos>=8 , 0 , 1)]
  dataset[ , mcuentas_saldo_cat   := ifelse (mcuentas_saldo   >= 119486, 0 , 1)]    
  dataset[ , cliente_edad_cat     := ifelse (cliente_edad  >= 55, 0 , 1)]
  dataset[ , mv_delinquency       := ifelse (Master_delinquency==0 |  Visa_delinquency==0, 1 , 0) ]
  dataset[ , mv_mlimitecompra     := ifelse (Master_mlimitecompra<=503734 |  Visa_mlimitecompra<=503734, 0 ,1)]
  dataset[ , cliente_antiguedad_cat := ifelse (cliente_antiguedad  >= 185, 0 , 1)]
 
   #combino MasterCard y Visa de manera loca
  dataset[ , mv_mfinanciacion_limite := rowSums( cbind( Master_mfinanciacion_limite,  Visa_mfinanciacion_limite) , na.rm=TRUE ) ]
  dataset[ , mv_msaldopesos          := rowSums( cbind( Master_msaldopesos,  Visa_msaldopesos) , na.rm=TRUE ) ]
  dataset[ , mv_msaldodolares        := rowSums( cbind( Master_msaldodolares,  Visa_msaldodolares) , na.rm=TRUE ) ]
  dataset[ , mv_mlimitecompra        := rowSums( cbind( Master_mlimitecompra,  Visa_mlimitecompra) , na.rm=TRUE ) ]
  dataset[ , mv_mconsumototal        := rowSums( cbind( Master_mconsumototal,  Visa_mconsumototal) , na.rm=TRUE ) ]
  dataset[ , mv_cconsumos            := rowSums( cbind( Master_cconsumos,  Visa_cconsumos) , na.rm=TRUE ) ]
  dataset[ , mv_mpagominimo          := rowSums( cbind( Master_mpagominimo,  Visa_mpagominimo) , na.rm=TRUE ) ]
  dataset[ , mv_msaldototal          := rowSums( cbind( Master_msaldototal,  Visa_msaldototal) , na.rm=TRUE ) ]
  dataset[ , mv_mconsumospesos        := rowSums( cbind( Master_mconsumospesos,  Visa_mconsumospesos) , na.rm=TRUE ) ]
  dataset[ , mv_mconsumosdolares        := rowSums( cbind( Master_mconsumosdolares,  Visa_mconsumosdolares) , na.rm=TRUE ) ]
  dataset[ , mv_madelantodolares       := rowSums( cbind( Master_madelantodolares,  Visa_madelantodolares) , na.rm=TRUE ) ]
  
  
   #ratio de variables conocidas y desconocidas
  dataset[ , r_prestamos_personales  := mprestamos_personales / cprestamos_personales]
  dataset[ , r_prestamos_hipotecarios:= mprestamos_hipotecarios / cprestamos_hipotecarios ]
  dataset[ , r_inversion1_dolares    := minversion1_dolares/ cinversion1 ]
  dataset[ , r_inversion1_pesos      := minversion1_pesos/ cinversion1]
  dataset[ , r_inversion2            := minversion2/ cinversion2 ]
  dataset[ , r_cajeros_propios_descuentos  := mcajeros_propios_descuentos/ ccajeros_propios_descuentos ]
  dataset[ , r_comisiones_mantenimiento := mcomisiones_mantenimiento/ ccomisiones_mantenimiento ]
  dataset[ , r_transferencias_recibidas := mtransferencias_recibidas/ ctransferencias_recibidas ]
  dataset[ , r_transferencias_emitidas := mtransferencias_emitidas/ ctransferencias_emitidas ]
  dataset[ , r_cheques_depositados := mcheques_depositados/ ccheques_depositados ]
  dataset[ , r_plazo_fijo := (mplazo_fijo_dolares+mplazo_fijo_pesos)/ cplazo_fijo ]
  
  dataset[ , mvr_msaldototal         := mv_msaldototal / mv_mlimitecompra ]
  dataset[ , mvr_msaldopesos2        := mv_msaldopesos / mv_msaldototal ]
  dataset[ , mvr_msaldodolares2      := mv_msaldodolares / mv_msaldototal ]
  dataset[ , mvr_mconsumospesos      := mv_mconsumospesos / mv_mlimitecompra ]
  dataset[ , mvr_mconsumosdolares    := mv_mconsumosdolares / mv_mlimitecompra ]
  dataset[ , mvr_madelantodolares    := mv_madelantodolares / mv_mlimitecompra ]


  #valvula de seguridad para evitar valores infinitos
  #paso los infinitos a NULOS
  infinitos      <- lapply(names(dataset),function(.name) dataset[ , sum(is.infinite(get(.name)))])
  infinitos_qty  <- sum( unlist( infinitos) )
  if( infinitos_qty > 0 )
  {
    cat( "ATENCION, hay", infinitos_qty, "valores infinitos en tu dataset. Seran pasados a NA\n" )
    dataset[mapply(is.infinite, dataset)] <- NA
  }


  #valvula de seguridad para evitar valores NaN  que es 0/0
  #paso los NaN a 0 , decision polemica si las hay
  #se invita a asignar un valor razonable segun la semantica del campo creado
  nans      <- lapply(names(dataset),function(.name) dataset[ , sum(is.nan(get(.name)))])
  nans_qty  <- sum( unlist( nans) )
  if( nans_qty > 0 )
  {
    cat( "ATENCION, hay", nans_qty, "valores NaN 0/0 en tu dataset. Seran pasados arbitrariamente a 0\n" )
    cat( "Si no te gusta la decision, modifica a gusto el programa!\n\n")
    dataset[mapply(is.nan, dataset)] <- 0
  }

  #FIN de la seccion donde se deben hacer cambios con variables nuevas

  columnas_extendidas <-  copy( setdiff(  colnames(dataset), columnas_originales ) )

  #grabo con nombre extendido
  fwrite( dataset,
          file=arch_destino,
          sep= "," )
}
#------------------------------------------------------------------------------

dir.create( "./datasets/" )


#lectura rapida del dataset  usando fread  de la libreria  data.table
dataset1  <- fread("./datasetsOri/paquete_premium_202011.csv")
dataset2  <- fread("./datasetsOri/paquete_premium_202101.csv")

EnriquecerDataset( dataset1, "./datasets/paquete_premium_202011_ext.csv" )
EnriquecerDataset( dataset2, "./datasets/paquete_premium_202101_ext.csv" )

quit( save="no")
