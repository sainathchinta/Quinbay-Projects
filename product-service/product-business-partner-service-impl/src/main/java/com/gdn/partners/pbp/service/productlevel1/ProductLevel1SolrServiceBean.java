package com.gdn.partners.pbp.service.productlevel1;

import com.gdn.mta.product.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.mta.product.service.config.KafkaPublisher;
import com.gdn.mta.domain.event.config.DomainEventName;
import com.gdn.mta.domain.event.modal.SolrProductCollectionUpdateEvent;
import com.gdn.mta.product.util.CommonUtils;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.repository.ProductCollectionRepository;
import com.gdn.mta.product.repository.SolrActiveProductCollectionRepository;
import com.gdn.mta.product.service.solr.SolrReviewProductCollectionService;
import com.gdn.mta.product.valueobject.SolrProductCollectionDTO;
import com.gdn.partners.pbp.service.productlevel1.ProductLevel1SolrService;
import lombok.extern.slf4j.Slf4j;

@Service
@Transactional(readOnly = true)
@Slf4j
public class ProductLevel1SolrServiceBean implements ProductLevel1SolrService {

  @Autowired
  private ProductCollectionRepository productCollectionRepository;

  @Autowired
  private SolrActiveProductCollectionRepository solrActiveProductCollectionRepository;

  @Autowired
  private SolrReviewProductCollectionService solrReviewProductCollectionService;

  @Autowired
  private KafkaPublisher kafkaProducer;

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void update(String productCode) throws Exception {
    String storeId = GdnMandatoryRequestParameterUtil.getStoreId();
    ProductCollection productCollection =
        this.productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, productCode);
    SolrProductCollectionDTO solrProductCollectionDTO = new SolrProductCollectionDTO();
    BeanUtils.copyProperties(productCollection, solrProductCollectionDTO);
    SolrProductCollectionUpdateEvent solrProductCollectionUpdateEvent =
        new SolrProductCollectionUpdateEvent(solrProductCollectionDTO);
    kafkaProducer.send(DomainEventName.SOLR_PRODUCT_COLLECTION_UPDATE,
        solrProductCollectionUpdateEvent.getSolrProductCollectionDTO().getProductCode(),
        solrProductCollectionUpdateEvent);
    if (productCollection.isReviewPending()) {
      solrReviewProductCollectionService.addProductToReviewProductCollection(productCollection);
    }
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void deleteByProductCollectionId(String productCollectionId) {
    try {
      SolrProductCollectionUpdateEvent solrProductCollectionUpdateEvent =
          CommonUtils.getSolrProductCollectionUpdateEvent(productCollectionId);
      kafkaProducer.send(DomainEventName.SOLR_PRODUCT_COLLECTION_UPDATE, productCollectionId,
          solrProductCollectionUpdateEvent);
    } catch (Exception e) {
      log.error("error while deleting productCollection solr document from solr core: " + "prd_collection. docId: {}",
          productCollectionId, e);
    }
  }

}
