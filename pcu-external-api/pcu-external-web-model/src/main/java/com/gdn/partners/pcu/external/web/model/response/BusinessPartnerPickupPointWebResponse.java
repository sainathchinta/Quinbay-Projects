package com.gdn.partners.pcu.external.web.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@JsonIgnoreProperties(ignoreUnknown = true)
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class BusinessPartnerPickupPointWebResponse {

  private static final long serialVersionUID = -5889710447579200759L;
  private String code;
  private String name;
  private String businessPartnerCode;
  private String address;
  private String countryCode;
  private String cityName;
  private String provinceCode;
  private String provinceName;
  private String cityCode;
  private String districtCode;
  private String districtName;
  private String subDistrictCode;
  private String subDistrictName;
  private String zipCode;
  private String telephone;
  private String fax;
  private String additionalInfo;
  private String contactPersonName;
  private String contactEmail;
  private String contactTelephone;
  private String contactTelephoneExt;
  private String coverageAreaSetting;
  private String warehouseId;
  private String locationId;
  private boolean activated = false;
  private String originId;
  private GeoLocationWebResponse geoLocationWebResponse;
  private String externalPickupPointCode;
  private boolean cncActivated;
  private boolean archived;
  private boolean defaultAddress;
  private Boolean hasBusinessHours;
  private String businessHourOption;
  private boolean fbbActivated;
  private boolean delivery;
}
