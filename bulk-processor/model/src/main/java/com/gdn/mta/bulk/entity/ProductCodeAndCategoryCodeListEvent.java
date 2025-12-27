package com.gdn.mta.bulk.entity;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@JsonIgnoreProperties(ignoreUnknown = true)
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class ProductCodeAndCategoryCodeListEvent extends GdnBaseDomainEventModel implements Serializable {

  private static final long serialVersionUID = -1884761788024286738L;
  List<ProductCodeAndCategoryCodeEvent> productCodeAndCategoryCodeEventList = new ArrayList<>();
}