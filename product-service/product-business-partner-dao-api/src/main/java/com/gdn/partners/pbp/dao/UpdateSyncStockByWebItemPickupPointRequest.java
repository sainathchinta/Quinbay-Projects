package com.gdn.partners.pbp.dao;

import com.gdn.x.inventory.v2.model.vo.request.InventoryRequestVo;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.codehaus.jackson.annotate.JsonIgnoreProperties;

import java.io.Serializable;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class UpdateSyncStockByWebItemPickupPointRequest extends InventoryRequestVo
  implements Serializable {
  private static final long serialVersionUID = 5367880046769492039L;
  private String webItemSku;
  private String webMerchantCode;
  private boolean syncStock;
  private String pickupPointCode;
  private Integer stock;
}
