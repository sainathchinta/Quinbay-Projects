package com.gdn.partners.pcu.external.web.model.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Set;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties
public class QuickEditV2WebRequest {

  private String itemSku;
  private String pickupPointCode;
  private Set<ProductLevel3PriceWebRequest> prices;
  private Integer deltaStock;
  private String status;
  private String cncStatus;
  private String sellerSku;
  private Boolean wholesalePriceActivated;
  private Boolean off2OnActiveFlag;
  private Boolean synchronizeStock;
  private Boolean cncActivated;
  private Long version;
  private B2bFieldsRequest b2bFields;
  private boolean scheduleRemoval;
}
