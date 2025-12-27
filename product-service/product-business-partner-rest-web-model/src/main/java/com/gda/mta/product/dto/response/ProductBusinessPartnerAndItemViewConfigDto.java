package com.gda.mta.product.dto.response;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.mta.product.entity.ProductBusinessPartner;
import com.gdn.x.product.rest.web.model.request.ItemViewConfigAndItemSkuRequest;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@JsonIgnoreProperties(ignoreUnknown = true)
@Data
@AllArgsConstructor
@NoArgsConstructor
public class ProductBusinessPartnerAndItemViewConfigDto {

  ProductBusinessPartner productBusinessPartner;
  List<ItemViewConfigAndItemSkuRequest> itemViewConfigAndItemSkuRequests;
}
