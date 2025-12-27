package com.gdn.partners.pcu.external.web.model.response;

import java.util.Date;

import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@AllArgsConstructor
@NoArgsConstructor
@ToString
@Builder
public class RequestFormResponse extends BaseResponse {
  private static final long serialVersionUID = -6268836029382470562L;
  private String requestFormNumber;
  private String itemSku;
  private String itemName;
  private String categoryCode;
  private String warehouseCode;
  private Date createdDate;
  private Date confirmedDate;
  private String status;
  private String transferFromItemSku;
  private String transferFromItemName;
  private String transferFromProductSku;
  private String productSku;
  private int usedStock;
  private boolean physicalBundle;
}

