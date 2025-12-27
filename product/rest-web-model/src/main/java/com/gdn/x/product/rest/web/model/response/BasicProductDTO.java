package com.gdn.x.product.rest.web.model.response;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.rest.web.model.dto.ItemCatalogDTO;
import com.gdn.x.product.rest.web.model.dto.MasterCatalogDTO;
import com.gdn.x.product.rest.web.model.dto.PreOrderDTO;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@JsonIgnoreProperties(ignoreUnknown = true)
@AllArgsConstructor
@NoArgsConstructor
@Data
public class BasicProductDTO {

  private String productSku;
  private String merchantCode;
  private ProductType productType;
  private PreOrderDTO preOrder;
  private MasterCatalogDTO masterCatalog;
  private List<ItemCatalogDTO> itemCatalogs = new ArrayList<ItemCatalogDTO>();
  private BasicMasterDataProductDTO masterDataProduct;
  private List<BasicProductAttributeDetailsDTO> descriptiveAttributes = new ArrayList<BasicProductAttributeDetailsDTO>();
  private String productCode;

}
