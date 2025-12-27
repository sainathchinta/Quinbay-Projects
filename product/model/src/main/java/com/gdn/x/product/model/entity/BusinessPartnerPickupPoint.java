package com.gdn.x.product.model.entity;

import java.util.ArrayList;
import java.util.List;

import org.springframework.data.mongodb.core.mapping.Document;
import org.springframework.data.mongodb.core.mapping.Field;

import com.gdn.x.product.enums.BusinessPartnerPickupPointFieldNames;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode(callSuper = false)
@Document(collection = "business_partner_pickup_point")
public class BusinessPartnerPickupPoint extends GdnBaseMongoEntity{

  private static final long serialVersionUID = 7093855068519716268L;

  @Field(value = BusinessPartnerPickupPointFieldNames.CODE)
  private String code;

  @Field(value = BusinessPartnerPickupPointFieldNames.NAME)
  private String name;

  @Field(value = BusinessPartnerPickupPointFieldNames.BUSINESS_PARTNER_CODE)
  private String businessPartnerCode;

  @Field(value = BusinessPartnerPickupPointFieldNames.ADDRESS)
  private String address;

  @Field(value = BusinessPartnerPickupPointFieldNames.COUNTRY_CODE)
  private String countryCode;

  @Field(value = BusinessPartnerPickupPointFieldNames.CITY_NAME)
  private String cityName;

  @Field(value = BusinessPartnerPickupPointFieldNames.PROVINCE_CODE)
  private String provinceCode;

  @Field(value = BusinessPartnerPickupPointFieldNames.PROVINCE_NAME)
  private String provinceName;

  @Field(value = BusinessPartnerPickupPointFieldNames.CITY_CODE)
  private String cityCode;

  @Field(value = BusinessPartnerPickupPointFieldNames.DISTRICT_CODE)
  private String districtCode;

  @Field(value = BusinessPartnerPickupPointFieldNames.DISTRICT_NAME)
  private String districtName;

  @Field(value = BusinessPartnerPickupPointFieldNames.SUB_DISTRICT_CODE)
  private String subDistrictCode;

  @Field(value = BusinessPartnerPickupPointFieldNames.SUB_DISTRICT_NAME)
  private String subDistrictName;

  @Field(value = BusinessPartnerPickupPointFieldNames.ZIP_CODE)
  private String zipCode;

  @Field(value = BusinessPartnerPickupPointFieldNames.TELEPHONE)
  private String telephone;

  @Field(value = BusinessPartnerPickupPointFieldNames.FAX)
  private String fax;

  @Field(value = BusinessPartnerPickupPointFieldNames.ADDITIONAL_INFO)
  private String additionalInfo;

  @Field(value = BusinessPartnerPickupPointFieldNames.CONTACT_PERSON_NAME)
  private String contactPersonName;

  @Field(value = BusinessPartnerPickupPointFieldNames.CONTACT_EMAIL)
  private String contactEmail;

  @Field(value = BusinessPartnerPickupPointFieldNames.CONTACT_TELEPHONE)
  private String contactTelephone;

  @Field(value = BusinessPartnerPickupPointFieldNames.CONTACT_TELEPHONE_EXT)
  private String contactTelephoneExt;

  @Field(value = BusinessPartnerPickupPointFieldNames.COVERAGE_AREA_SETTING)
  private String coverageAreaSetting;

  @Field(value = BusinessPartnerPickupPointFieldNames.WAREHOUSE_ID)
  private String warehouseId;

  @Field(value = BusinessPartnerPickupPointFieldNames.LOCATION_ID)
  private String locationId;

  @Builder.Default
  @Field(value = BusinessPartnerPickupPointFieldNames.ACTIVATED)
  private boolean activated = false;

  @Field(value = BusinessPartnerPickupPointFieldNames.ORIGIN_ID)
  private String originId;

  @Field(value = BusinessPartnerPickupPointFieldNames.GEO_LOCATION)
  private Geolocation geolocation;

  @Field(value = BusinessPartnerPickupPointFieldNames.EXTERNAL_PICKUP_POINT_CODE)
  private String externalPickupPointCode;

  @Field(value = BusinessPartnerPickupPointFieldNames.CNC_ACTIVATED)
  private boolean cncActivated;

  @Builder.Default
  @Field(value = BusinessPartnerPickupPointFieldNames.BUSINESS_HOURS)
  private List<BusinessHour> businessHours = new ArrayList<>();

  @Field(value = BusinessPartnerPickupPointFieldNames.ARCHIVED)
  private boolean archived;

  @Field(value = BusinessPartnerPickupPointFieldNames.DEFAULT_ADDRESS)
  private boolean defaultAddress;

  @Field(value = BusinessPartnerPickupPointFieldNames.HAS_BUSINESS_HOURS)
  private Boolean hasBusinessHours;

  @Field(value = BusinessPartnerPickupPointFieldNames.BUSINESS_HOUR_OPTION)
  private String businessHourOption;

  @Field(value = BusinessPartnerPickupPointFieldNames.FBB_ACTIVATED)
  private boolean fbbActivated;

  @Field(value = BusinessPartnerPickupPointFieldNames.DELIVERY)
  private boolean delivery;

  @Field(value = BusinessPartnerPickupPointFieldNames.DISTRIBUTION)
  private boolean distribution;
}
