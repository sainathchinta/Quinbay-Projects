package com.gdn.x.product.service.impl;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.x.product.dao.api.DeferredSolrReindexItemRepository;
import com.gdn.x.product.enums.ProductReindexStatus;
import com.gdn.x.product.model.entity.DeferredSolrReindexItem;
import com.gdn.x.product.service.api.DeferredSolrReindexItemService;
import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Objects;

@Service
public class DeferredSolrReindexItemServiceImpl implements DeferredSolrReindexItemService {

  @Autowired
  private DeferredSolrReindexItemRepository deferredSolrReindexItemRepository;

  private static final String DEFERRED_SOLR_REINDEX_ITEM_MUST_NOT_BE_NULL = "deferred solr reindex items are null or empty";

  @Override
  public List<DeferredSolrReindexItem> save(List<DeferredSolrReindexItem> deferredSolrReindexItems) {
    GdnPreconditions.checkArgument(
        CollectionUtils.isNotEmpty(deferredSolrReindexItems), DEFERRED_SOLR_REINDEX_ITEM_MUST_NOT_BE_NULL);
    return deferredSolrReindexItemRepository.saveAll(deferredSolrReindexItems);
  }

  @Override
  public DeferredSolrReindexItem save(DeferredSolrReindexItem deferredSolrReindexItem) {
    GdnPreconditions.checkArgument(
        Objects.nonNull(deferredSolrReindexItem), DEFERRED_SOLR_REINDEX_ITEM_MUST_NOT_BE_NULL);
    return deferredSolrReindexItemRepository.save(deferredSolrReindexItem);
  }

  @Override
  public Page<DeferredSolrReindexItem> findByStoreId(String storeId, Pageable pageable) {
    return deferredSolrReindexItemRepository.findByStoreIdAndMarkForDeleteFalseOrderByUpdatedDate(storeId, pageable);
  }

  @Override
  public List<DeferredSolrReindexItem> findByStoreIdAndReindexTypeAndProductReindexStatus(String storeId,
      String reindexType, ProductReindexStatus productReindexStatus, Pageable pageable) {
    return deferredSolrReindexItemRepository.findByStoreIdAndReindexTypeAndProductReindexStatusAndMarkForDeleteFalse(
        storeId, reindexType, productReindexStatus, pageable);
  }

  @Override
  public List<DeferredSolrReindexItem> findByStoreIdAndItemSkuIn(String storeId, List<String> itemSkus) {
    return deferredSolrReindexItemRepository.findByStoreIdAndItemSkuIn(storeId, itemSkus);
  }

}
