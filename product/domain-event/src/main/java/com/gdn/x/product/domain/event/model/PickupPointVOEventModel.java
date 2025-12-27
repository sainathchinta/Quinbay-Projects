package com.gdn.x.product.domain.event.model;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class PickupPointVOEventModel {
  private String code;
  private String businessPartnerCode;
  private String storeId;
  private String externalPickupPointCode;
  private String stockholmWarehouseCode;
  private String name;
  private String nameAlias;
  private String generatedName;
  private String displayName;
  private boolean usingGeneratedName;
  private boolean fbbActivated;
  private boolean defaultWarehouse;
  private String address;
  private String addressAlias;
  private String originalAddress;
  private String originalName;
  private String countryCode;
  private String countryName;
  private String provinceCode;
  private String cityCode;
  private String districtCode;
  private String subDistrictCode;
  private String zipCode;
  private String provinceName;
  private String cityName;
  private String districtName;
  private String subDistrictName;
  private String telephone;
  private String warehouseId;
  private String originId;
  private GeolocationVOEventModel geolocation;
  private ContactPersonVOEventModel contactPerson;
  private String coverageAreaSetting;
  private String fax;
  private String additionalInfo;
  private String locationId;
  private boolean cncActivated;
  private boolean defaultAddress;
  private boolean returnAddressFlag;
  private List<BusinessHourVOEventModel> businessHours;
  private boolean activated;
  private boolean archived;
  private Boolean hasBusinessHours;
  private Boolean internationalShipping;
  private Boolean pickupScheduleMandatory;
  private String displayLabelNameEn;
  private String displayLabelNameId;
  private PickupPointFlagVOEventModel flags;
}
