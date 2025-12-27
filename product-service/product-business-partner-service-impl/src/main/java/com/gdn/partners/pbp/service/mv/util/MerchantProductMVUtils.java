package com.gdn.partners.pbp.service.mv.util;

import java.util.Date;

import org.apache.commons.collections4.CollectionUtils;

import com.gdn.partners.pbp.entity.mv.MerchantProductMV;
import com.gdn.partners.pbp.util.ProductLevel3Util;
import com.gdn.x.product.enums.ChannelName;
import com.gdn.x.product.rest.web.model.dto.CategoryDTO;
import com.gdn.x.product.rest.web.model.dto.ItemSummaryResponse;
import com.gdn.x.product.rest.web.model.dto.ItemViewConfigDTO;

public interface MerchantProductMVUtils {

  public static void populateMaterializedViewProperties(MerchantProductMV mv,
      ItemSummaryResponse item) {
    mv.setStoreId(item.getStoreId());
    mv.setCreatedDate(item.getCreatedDate());
    mv.setCreatedBy(item.getCreatedBy());
    mv.setUpdatedDate(item.getUpdatedDate());
    mv.setUpdatedBy(item.getUpdatedBy());
    mv.setBusinessPartnerCode(item.getMerchantCode());
    mv.setItemSku(item.getItemSku());
    mv.setName(item.getGeneratedItemName());
    mv.setMerchantSku(item.getMerchantSku());
    mv.setArchived(item.isArchived());
    mv.setMarkForDelete(item.isMarkForDelete());
    mv.setLastIndexedProductDate(new Date());
    mv.setPickupPointCode(item.getPickupPointCode());

    if (item.getMasterCatalog() != null && item.getMasterCatalog().getCategory() != null) {
      CategoryDTO itemCategory = item.getMasterCatalog().getCategory();
      mv.setCategoryCode(itemCategory.getCategoryCode());
    }

    if (CollectionUtils.isNotEmpty(item.getItemViewConfigs())) {
      ItemViewConfigDTO itemViewConfig =
          ProductLevel3Util.getItemViewConfigDTO(item.getItemViewConfigs(), ChannelName.DEFAULT);
      if (itemViewConfig != null) {
        mv.setBuyable(itemViewConfig.isBuyable());
        mv.setDisplayable(itemViewConfig.isDiscoverable());
      }
    }
  }
}
