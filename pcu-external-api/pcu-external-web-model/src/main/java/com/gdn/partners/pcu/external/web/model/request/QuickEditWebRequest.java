package com.gdn.partners.pcu.external.web.model.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;
import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
@ToString
@JsonIgnoreProperties(ignoreUnknown = true)
public class QuickEditWebRequest {
  private String itemSku;
  private List<ProductLevel3PriceWebRequest> prices;
  private Integer deltaStock;
  private String status;
  private Boolean wholesalePriceActivated;
  private Boolean off2OnActiveFlag;
  private Boolean synchronizeStock;
  private Long version;
  private String pickupPointCode;
}
