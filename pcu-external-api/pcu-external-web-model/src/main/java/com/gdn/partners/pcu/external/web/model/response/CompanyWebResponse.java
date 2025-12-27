package com.gdn.partners.pcu.external.web.model.response;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

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
public class CompanyWebResponse {
  private String businessPartnerName;
  private String name;
  private String description;
  private String address1;
  private String address2;
  private String countryName;
  private String provinceName;
  private String cityName;
  private String districtName;
  private String subDistrictName;
  private String zipCode;
  private String countryCode;
  private String provinceCode;
  private String cityCode;
  private String districtCode;
  private String subDistrictCode;
  private String email;
  private String phone;
  private String fax;
  private String typeOfBusiness;
  private String longStanding;
  private String website;
  private boolean supplierFlag;
  private boolean customerFlag;
  private boolean merchantFlag;
  private boolean internationalFlag;
  private String imageLogo;
  private boolean contractSet;
  private String purchaseTerm;
  private String inventoryFulfillment;
  private String merchantType;
  private int salesLevel;
  private boolean anchor;
  private String termOfPayment;
  private String merchantDeliveryType;
  private boolean offlineToOnlineFlag;
  private boolean cncActivated;
  private String referralCode;
  private String businessPartnerAlias;
  private Double depositAmount;
  private Boolean penaltyFlag;
  private Date resignDate;
  private List<String> salesChannel = new ArrayList<>();
}
