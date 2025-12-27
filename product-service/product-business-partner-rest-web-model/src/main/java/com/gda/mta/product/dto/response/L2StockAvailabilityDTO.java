package com.gda.mta.product.dto.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

import java.io.Serializable;

@Data
@ToString
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class L2StockAvailabilityDTO extends BaseResponse implements Serializable {
  private static final long serialVersionUID = -4794942872269546834L;
  String warehouseItemSku;
  boolean distributionWarehouseAvailable;
  boolean nonDistributionWarehouseAvailable;
}
