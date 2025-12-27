package com.gdn.x.product.rest.web.model.response;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.model.entity.MasterDataProductAttribute;
import com.gdn.x.product.model.entity.PreOrder;
import com.gdn.x.product.model.entity.ProductSpecialAttribute;
import com.gdn.x.product.model.vo.ItemCatalogVO;
import com.gdn.x.product.rest.web.model.dto.MasterCatalogDTO;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@JsonIgnoreProperties(ignoreUnknown = true)
@AllArgsConstructor
@NoArgsConstructor
@Data
public class BasicProductAndItemDTO {

  private String productSku;
  private String merchantCode;
  private ProductType productType;
  private MasterCatalogDTO masterCatalog = new MasterCatalogDTO();
  private BasicMasterDataProductDTO masterDataProduct = new BasicMasterDataProductDTO();
  private BasicItemDTO item;
  private PreOrder preOrder;
  private List<ItemCatalogVO> itemCatalogVOS;
  private List<MasterDataProductAttribute> masterDataProductAttributes;
  private List<ProductSpecialAttribute> productSpecialAttributesList = new ArrayList<>();
  private String productCode;

}
