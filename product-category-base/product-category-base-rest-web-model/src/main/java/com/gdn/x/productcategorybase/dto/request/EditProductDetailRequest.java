package com.gdn.x.productcategorybase.dto.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.productcategorybase.entity.Product;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import lombok.ToString;

import java.io.Serializable;
import java.security.SecureRandom;
import java.util.ArrayList;
import java.util.List;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@ToString
@JsonIgnoreProperties(ignoreUnknown = true)
public class EditProductDetailRequest implements Serializable {
  private static final long serialVersionUID = 8681887364943427088L;
  ProductRequest productRequest;
  List<ProductItemUpcCodeUpdateRequest> productItemUpcCodeUpdateRequestList = new ArrayList<>();
  List<ProductImageEditRequest> productImageEditRequests = new ArrayList<>();
  boolean resetExtractedAttributeValue;
}
