package com.gdn.partners.pbp.service.mv.updater;

import java.util.Date;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.gdn.mta.product.repository.ProductLevel3Repository;
import com.gdn.partners.pbp.entity.mv.MerchantProductMV;
import com.gdn.partners.pbp.entity.mv.MerchantProductMVIndexingFailed;
import com.gdn.partners.pbp.repository.eventstore.MerchantProductMVIndexingFailedRepository;
import com.gdn.partners.pbp.service.mv.util.MerchantProductMVUtils;
import com.gdn.partners.pbp.util.ProductLevel3Util;
import com.gdn.x.product.domain.event.model.ItemChange;
import com.gdn.x.product.domain.event.model.ItemViewConfig;
import com.gdn.x.product.domain.event.model.MasterDataItem;
import com.gdn.x.product.enums.ChannelName;
import com.gdn.x.product.rest.web.model.dto.ItemSummaryResponse;

@Service("merchantProductMVProductItemUpdaterService")
public class MerchantProductMVProductItemUpdaterServiceBean extends
    BaseMerchantProductMVProductItemUpdaterService<ItemChange> {

  @Autowired
  private ProductLevel3Repository productLevel3Repository;

  @Autowired
  private MerchantProductMVIndexingFailedRepository merchantProductMVIndexingFailedRepository;

  private static final Logger LOG = LoggerFactory
      .getLogger(MerchantProductMVProductItemUpdaterServiceBean.class);

  private void saveFailedProductItem(String itemSku) {
    try {
      MerchantProductMVIndexingFailed savedFailedData =
          merchantProductMVIndexingFailedRepository.findByItemSku(itemSku);
      if (savedFailedData == null) {
        savedFailedData = new MerchantProductMVIndexingFailed();
        savedFailedData.setItemSku(itemSku);
        merchantProductMVIndexingFailedRepository.save(savedFailedData);
      }
    } catch (Exception e) {
      LOG.error("Error when saving item sku to failed item skus. ItemSku: {}", itemSku, e);
    }
  }

  @Override
  protected MerchantProductMV getMaterializedView(ItemChange eventModel) {
    return merchantProductMVRepository.findByStoreIdAndItemSku(eventModel.getStoreId(),
        eventModel.getItemSku());
  }

  @Override
  protected void updateMaterializedView(MerchantProductMV mv, ItemChange eventModel) {
    mv.setPickupPointCode(eventModel.getPickupPointCode());
    mv.setMerchantSku(eventModel.getMerchantSku());
    mv.setMarkForDelete(eventModel.isMarkForDelete());
    mv.setLastIndexedProductDate(new Date());
    mv.setArchived(eventModel.isArchived());

    MasterDataItem masterDataItem = eventModel.getMasterDataItem();
    if (masterDataItem != null) {
      mv.setName(masterDataItem.getGeneratedItemName());
    }

    ItemViewConfig itemViewConfig =
        ProductLevel3Util.getItemViewConfig(eventModel.getItemViewConfigs(), ChannelName.DEFAULT);
    if (itemViewConfig != null) {
      mv.setBuyable(itemViewConfig.isBuyable());
      mv.setDisplayable(itemViewConfig.isDiscoverable());
    }
    this.merchantProductMVRepository.save(mv);
  }

  @Override
  protected void handleNotFoundMaterializedView(ItemChange eventModel) {
    MerchantProductMV mv = new MerchantProductMV();
    try {
      ItemSummaryResponse item =
          productLevel3Repository.findSummaryByGdnSku(eventModel.getItemSku());
      MerchantProductMVUtils.populateMaterializedViewProperties(mv, item);
      merchantProductMVRepository.saveAndFlush(mv);
      LOG.info("Succeed add {} to materialized view", eventModel.getItemSku());
    } catch (Exception e) {
      saveFailedProductItem(eventModel.getItemSku());
      LOG.error(
          "Error when trying to add item to materialized view. Added to failed item skus, please reindex later. ItemSku: {}",
          eventModel.getItemSku(), e);
    }
  }
}
