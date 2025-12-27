package com.gdn.x.product.service.impl;

import static com.gdn.common.base.GdnPreconditions.checkArgument;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.product.dao.solr.api.ProductAndItemSolrRepository;
import com.gdn.x.product.model.solr.ProductAndItemSolr;
import com.gdn.x.product.model.vo.ProductAndItemsVO;
import com.gdn.x.product.service.api.Off2OnHelperService;
import com.gdn.x.product.service.api.SaveOperationService;

@Service
public class Off2OnHelperServiceImpl implements Off2OnHelperService {

  private static final Logger LOG = LoggerFactory.getLogger(Off2OnHelperServiceImpl.class);

  private static final String STORE_ID_MUST_NOT_BE_BLANK = "storeId must not be blank";

  private static final String ITEM_SKU_MUST_NOT_BE_BLANK = "itemSku must not be blank";

  private static final String PRODUCT_SKU_MUST_NOT_BE_BLANK = "productSku must not be blank";

  @Autowired
  private SaveOperationService saveOperationService;

  @Autowired
  private ProductAndItemSolrRepository productAndItemSolrRepository;

  @Override
  public List<String> changeOff2OnChannelActiveByItemSku(String storeId, List<String> itemSkus,
      boolean active) {
    List<String> failedList = new ArrayList<String>();
    for (String itemSku : itemSkus) {
      boolean success = false;
      try {
        success = this.changeOff2OnChannelActiveByItemSku(storeId, itemSku, active);
      } catch (ApplicationRuntimeException e) {
        Off2OnHelperServiceImpl.LOG.error(
            "#changeActivation by itemSku to {} failed with itemSku : {}, error : {}", active,
            itemSku, e.getMessage());
      } catch (Exception e) {
        Off2OnHelperServiceImpl.LOG.error(
            "#changeActivation by itemSku to {} failed with itemSku : {}, error : {}", active,
            itemSku, e.getMessage(), e);
      }
      if (!success) {
        failedList.add(itemSku);
      }
    }
    return failedList;
  }

  @Override
  public boolean changeOff2OnChannelActiveByItemSku(String storeId, String itemSku, boolean active) {
    checkArgument(StringUtils.isNotBlank(storeId),
        Off2OnHelperServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(itemSku),
        Off2OnHelperServiceImpl.ITEM_SKU_MUST_NOT_BE_BLANK);
    ProductAndItemsVO updateResult =
        this.saveOperationService.updateOff2OnChannelActiveByItemSku(storeId, itemSku, active);
    return (updateResult != null);
  }

  @Override
  public List<String> changeOff2OnChannelActiveByMerchantCode(String storeId, String merchantCode,
      boolean active) {
    Set<ProductAndItemSolr> productAndItemSolr =
        this.productAndItemSolrRepository
            .findByStoreIdAndMerchantCodeAndOff2OnChannelActiveAndMarkForDeleteFalse(storeId,
                merchantCode, !active);
    List<String> productSkus = new ArrayList<String>();
    for (ProductAndItemSolr solrObject : productAndItemSolr) {
      productSkus.add(solrObject.getProductSku());
    }
    return this.changeOff2OnChannelActiveByProductSku(storeId, productSkus, active);
  }

  @Override
  public List<String> changeOff2OnChannelActiveByProductSku(String storeId,
      List<String> productSkus, boolean active) {
    List<String> failedList = new ArrayList<String>();
    for (String productSku : productSkus) {
      boolean success = false;
      try {
        success = this.changeOff2OnChannelActiveByProductSku(storeId, productSku, active);
      } catch (ApplicationRuntimeException e) {
        Off2OnHelperServiceImpl.LOG.error(
            "#activateOff2OnChannelByProductSku failed with itemSku : {}, error : {}", productSku,
            e.getMessage());
      } catch (Exception e) {
        Off2OnHelperServiceImpl.LOG.error(
            "#activateOff2OnChannelByProductSku failed with itemSku : {}, error : {}", productSku,
            e.getMessage(), e);
      }
      if (!success) {
        failedList.add(productSku);
      }
    }
    return failedList;
  }

  @Override
  public boolean changeOff2OnChannelActiveByProductSku(String storeId, String productSku,
      boolean active) {
    checkArgument(StringUtils.isNotBlank(storeId),
        Off2OnHelperServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(productSku),
        Off2OnHelperServiceImpl.PRODUCT_SKU_MUST_NOT_BE_BLANK);
    ProductAndItemsVO updateResult =
        this.saveOperationService
            .updateOff2OnChannelActiveByProductSku(storeId, productSku, active);
    return (updateResult != null);
  }

}
