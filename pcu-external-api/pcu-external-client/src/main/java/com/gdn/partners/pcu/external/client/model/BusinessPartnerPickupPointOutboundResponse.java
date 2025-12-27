package com.gdn.partners.pcu.external.client.model;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

import com.gdn.common.web.base.BaseResponse;
import com.gdn.partners.bp.dto.pickuppoint.PickupSchedulesDTO;
import com.gdn.x.businesspartner.commons.enums.PickupScheduleStatus;
import com.gdn.x.businesspartner.dto.BusinessHourDTO;
import com.gdn.x.businesspartner.dto.GeolocationDTO;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class BusinessPartnerPickupPointOutboundResponse extends BaseResponse implements Serializable {
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
  private GeolocationDTO geolocation;
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
