package com.gdn.partners.pcu.external.web.model.response;

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
public class BusinessPartnerProfileWebResponse {
  private String businessPartnerCode;
  private String businessPartnerType;
  private List<PickupPointWebResponse> pickupPoints;
  private boolean activated;
  private boolean viewable;
  private boolean markForDelete;
  private String merchantStatus;
  private Date activationDate;
  private String businessPartnerName;
  private String id;
  private boolean cncActivated;
  private CompanyWebResponse company;
  private ProductSettingWebResponse productSettings;
  private Boolean multiDefaultAddressFlag;
  private boolean bigProductFlag = true;
  private boolean bopisFlag = true;
  private boolean official;
  private boolean productLimitEnabled;
  private boolean faasActivated;
  private boolean distributionSeller;
  private boolean productConsequenceLimitation;
}
