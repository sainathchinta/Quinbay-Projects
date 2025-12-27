package com.gdn.x.product.model.vo;

import java.io.Serializable;
import java.util.Date;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude
public class PreOrderVO implements Serializable {
  private static final long serialVersionUID = -8491713381144784251L;

  private Boolean isPreOrder;
  private String preOrderType;
  private Integer preOrderValue;
  private Date preOrderDate;
}
