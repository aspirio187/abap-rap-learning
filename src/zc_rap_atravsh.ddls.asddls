@Metadata.allowExtensions: true
@Metadata.ignorePropagatedAnnotations: true
@EndUserText: {
  label: 'SHGENERATED Core Data Service Entity'
}
@Search.searchable: true
@ObjectModel: {
  sapObjectNodeType.name: 'ZRAP_ATRAVSH'
}
@AccessControl.authorizationCheck: #MANDATORY
define root view entity ZC_RAP_ATRAVSH
  provider contract transactional_query
  as projection on ZR_RAP_ATRAVSH
  association [1..1] to ZR_RAP_ATRAVSH as _BaseEntity on $projection.TravelID = _BaseEntity.TravelID
{
      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.90
  key TravelID,
      @Search.defaultSearchElement: true
      @ObjectModel.text.element: [ 'AgencyName' ]
      @Consumption.valueHelpDefinition: [{
          entity: {
              name: '/DMO/I_Agency_StdVH',
              element: 'AgencyID'
          },
          useForValidation: true
       }]
      AgencyID,
      @Search.defaultSearchElement: true
      @ObjectModel.text.element: [ 'AgencyName' ]
      @Consumption.valueHelpDefinition: [{
            entity: {
                name: '/DMO/I_Customer_StdVH',
                element: 'CustomerID'
            },
            useForValidation: true
      }]
      _Agency.Name              as AgencyName,
      @Search.defaultSearchElement: true
      @ObjectModel.text.element: [ 'CustomerName' ]
      @Consumption.valueHelpDefinition: [{
            entity: {
                name: '/DMO/I_Customer_StdVH',
                element: 'CustomerID'
            },
            useForValidation: true
      }]
      CustomerID,
      _Customer.LastName        as CustomerName,
      BeginDate,
      EndDate,
      @Semantics: {
        amount.currencyCode: 'CurrencyCode'
      }
      BookingFee,
      @Semantics: {
        amount.currencyCode: 'CurrencyCode'
      }
      TotalPrice,
      @Consumption: {
        valueHelpDefinition: [ {
          entity : {
            element: 'Currency',
            name: 'I_CurrencyStdVH'
          },
          useForValidation: true
        } ]
      }
      CurrencyCode,
      Description,
      @ObjectModel.text.element: [ 'OverallStatusText' ]
      @Consumption.valueHelpDefinition: [{
            entity: {
                name: '/DMO/I_Overall_Status_VH',
                element: 'OverallStatus'
            },
            useForValidation: true
      }]
      OverallStatus,
      _OverallStatus._Text.Text as OverallStatusText : localized,
      @Semantics.largeObject: {
          mimeType: 'MimeType',
          fileName: 'FileName'
      }
      Attachment,
      MimeType,
      FileName,
      @Semantics: {
        user.createdBy: true
      }
      CreatedBy,
      @Semantics: {
        systemDateTime.createdAt: true
      }
      CreatedAt,
      @Semantics: {
        user.localInstanceLastChangedBy: true
      }
      LocalLastChangedBy,
      @Semantics: {
        systemDateTime.localInstanceLastChangedAt: true
      }
      LocalLastChangedAt,
      @Semantics: {
        systemDateTime.lastChangedAt: true
      }
      LastChangedAt,
      _BaseEntity
}
