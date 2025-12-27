package com.gdn.x.product.rest.web.model.dto;

import java.io.Serial;
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
public class PreOrderDTO implements Serializable {

  @Serial
  private static final long serialVersionUID = 3195229699259375652L;

  private Boolean isPreOrder;
  private String preOrderType;
  private Integer preOrderValue;
  private Date preOrderDate;
  private boolean convertToJKT;
}