package com.gdn.x.product.domain.event.model;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import com.gdn.x.product.model.vo.PreOrderVO;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@AllArgsConstructor
@NoArgsConstructor
@Data
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductPreOrderStatus extends GdnBaseDomainEventModel implements Serializable {

  private static final long serialVersionUID = -2333438941250162337L;

  private String productCode;
  private String productSku;
  private PreOrderVO preOrder;
}
