package com.gda.mta.product.dto;

import com.gdn.x.businesspartner.dto.ProfileResponse;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Date;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class UpdateOrInsertStockVo {
  private String businessPartnerCode;
  private String productSku;
  private String itemSku;
  private String itemCode;
  private String pickupPointCode;
  private int stock;
  private int initialPreOrderQuota;
  private boolean updatePreOrderQuota;
  private int minimumStock;
  private boolean syncStock;
  private boolean fbbActive;
  private boolean mppForWhEnabled;
  private ProfileResponse profileResponse;
  private Date preOrderDate;
  private boolean distributionPickupPoint;
}
