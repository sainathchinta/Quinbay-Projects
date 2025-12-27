package com.gdn.x.product.rest.web.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductImageDTO;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.io.Serializable;
import java.util.List;


@JsonIgnoreProperties(ignoreUnknown = true)
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class MasterDataProductV2 implements Serializable{
  private static final long serialVersionUID = 4972551207665699530L;
  private String brand;
  private double shippingWeight;
  private String productName;
  private List<MasterDataProductImageDTO> masterDataProductImages;
}