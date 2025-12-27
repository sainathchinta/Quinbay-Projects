package com.gdn.x.product.rest.web.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@JsonIgnoreProperties(ignoreUnknown = true)
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class BusinessPartnerPickupPointResponse extends BaseResponse {

  private static final long serialVersionUID = -165949595481420025L;

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
  private GeoLocationResponse geolocation;
  private String externalPickupPointCode;
  private boolean cncActivated;
  private boolean archived;
  private boolean defaultAddress;
  private Boolean hasBusinessHours;
  private String businessHourOption;
  private boolean fbbActivated;
  private boolean delivery;
  private boolean distribution;
}