package com.gdn.partners.pcu.external.client.model;

import com.gdn.common.web.base.BaseResponse;
import com.gdn.partners.bp.dto.pickuppoint.PickupSchedulesDTO;
import com.gdn.x.businesspartner.commons.enums.PickupScheduleStatus;
import com.gdn.x.businesspartner.dto.BusinessHourDTO;
import com.gdn.x.businesspartner.dto.GeolocationDTO;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class PickupPointOutboundResponse extends BaseResponse implements Serializable {
  private static final long serialVersionUID = 3877193446340471216L;
  private String code;
  private String stockholmWarehouseCode;
  private String name;
  private String nameAlias;
  private String generatedName;
  private String displayName;
  private String businessPartnerCode;
  private String businessPartnerName;
  private boolean activated;
  private boolean fbbActivated;
  private boolean defaultWarehouse;
  private String additionalInfo;
  private String address1;
  private String address2;
  private String addressAlias;
  private List<BusinessHourDTO> businessHours;
  private List<PickupSchedulesDTO> pickupSchedules;
  private PickupScheduleStatus pickupScheduleStatus;
  private String cityCode;
  private String cityName;
  private boolean cncActivated;
  private String contactEmail;
  private String contactPersonName;
  private String contactTelephone;
  private String contactTelephoneExt;
  private String countryCode;
  private String countryName;
  private String coverageAreaSetting;
  private String districtCode;
  private String districtName;
  private String externalPickupPointCode;
  private String fax;
  private GeolocationDTO geolocation;
  private String locationId;
  private String originId;
  private String provinceCode;
  private String provinceName;
  private String subDistrictCode;
  private String subDistrictName;
  private String telephone;
  private String warehouseId;
  private String zipCode;
  private boolean defaultAddress;
  private boolean returnAddressFlag;
  private boolean archived;
  private Boolean hasBusinessHours;
  private String businessHourOption;
  private Boolean internationalShipping;
  private boolean usingGeneratedName = false;
  private Boolean pickupScheduleMandatory;
  private String displayLabelNameEn;
  private String displayLabelNameId;
  private boolean waitingDeletion;
  private Date waitingDeletionInitiationDate;
  private String productDeletionStatus;
  private String originalAddress;
  private String originalName;
  private FlagDTO flags;
}
