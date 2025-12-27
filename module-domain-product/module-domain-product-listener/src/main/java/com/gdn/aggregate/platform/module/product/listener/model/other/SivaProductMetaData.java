package com.gdn.aggregate.platform.module.product.listener.model.other;

import com.fasterxml.jackson.annotation.JsonInclude;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;

@Data
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude(JsonInclude.Include.NON_NULL)
public class SivaProductMetaData {

  private Double salePrice;

  private Boolean isInStock;

  private String source;

}
