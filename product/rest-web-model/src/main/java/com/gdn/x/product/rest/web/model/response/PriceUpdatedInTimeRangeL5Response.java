package com.gdn.x.product.rest.web.model.response;

import java.io.Serializable;
import java.util.Date;

import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class PriceUpdatedInTimeRangeL5Response extends BaseResponse implements Serializable {
  private static final long serialVersionUID = -4570980661406145151L;
  private String itemSku;
  private String pickupPointCode;
  private Date priceUpdatedDate;
  private boolean buyable;
  private boolean discoverable;
  private boolean cncActive;
  private boolean markForDelete;
}
