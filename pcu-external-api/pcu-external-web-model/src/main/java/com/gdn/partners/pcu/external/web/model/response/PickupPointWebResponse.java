package com.gdn.partners.pcu.external.web.model.response;

import java.util.Date;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude(JsonInclude.Include.ALWAYS)
@JsonIgnoreProperties(ignoreUnknown = true)
public class PickupPointWebResponse {
  private String id;
  private String name;
  private String businessPartnerCode;
  private String code;
  private String address1;
  private String address2;
  private String cityName;
  private String provinceName;
  private String districtName;
  private String subDistrictName;
  private String zipCode;
  private String telephone;
  private String fax;
  private String additionalInfo;
  private String contactPersonName;
  private String contactTelephone;
  private String contactTelephoneExt;
  private String coverageAreaSetting;
  private String warehouseId;
  private String locationId;
  private boolean activated;
  private String originId;
  private String provinceCode;
  private String cityCode;
  private String districtCode;
  private String subDistrictCode;
  private String countryCode;
  private boolean cncActivated;
  private String externalPickupPointCode;
  private boolean defaultAddress;
  private GeolocationDTOResponse geolocation;
  private boolean fbbActivated;

}
