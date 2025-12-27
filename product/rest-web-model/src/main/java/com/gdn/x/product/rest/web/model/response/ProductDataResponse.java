package com.gdn.x.product.rest.web.model.response;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.rest.web.model.dto.ProductAttributeDTO;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductDataResponse implements Serializable {
  private static final long serialVersionUID = -1161052721641354495L;
  private String productCode;
  private ProductType productType;
  private String productName;
  private boolean suspended;
  private String halal;
  private String storage;
  private List<ProductAttributeDTO> definingAttributes = new ArrayList<>();
}
