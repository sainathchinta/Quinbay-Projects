package com.gdn.mta.product.service.solr;

import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.gdn.mta.domain.event.config.DomainEventName;
import com.gdn.mta.product.service.config.KafkaPublisher;
import com.gdn.mta.product.util.CommonUtils;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.domain.event.modal.SolrProductCollectionUpdateEvent;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.repository.ProductCollectionRepository;
import com.gdn.mta.product.repository.SolrActiveProductCollectionRepository;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class SolrActiveProductCollectionServiceBean implements SolrActiveProductCollectionService {

  @Autowired
  private SolrActiveProductCollectionRepository solrActiveProductCollectionRepository;

  @Autowired
  private ProductCollectionRepository productCollectionRepository;

  @Autowired
  private KafkaPublisher kafkaProducer;

  @Override
  public void deleteFromSolrProductCollection(String storeId, String productCode) {
    ProductCollection productCollection =
        this.productCollectionRepository.findByStoreIdAndProductCode(storeId, productCode);
    if (productCollection != null) {
      deleteSolrProductCollectionDocument(productCollection.getId());
      productCollection.setUpdatedDate(new Date());
      productCollection
          .setUpdatedBy(MDC.get(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER));
      this.productCollectionRepository.save(productCollection);
    }
  }

  @Override
  public void addSolrProductCollectionDocument(SolrProductCollectionUpdateEvent solrProductCollectionUpdateEvent) {
    this.solrActiveProductCollectionRepository.addSolrProductCollectionDocument(solrProductCollectionUpdateEvent.getSolrProductCollectionDTO());
  }

  @Override
  public void syncInactiveProductCollection(String storeId) {
    List<String> products = this.productCollectionRepository.findInactiveProducts(storeId);
    products.stream().forEach(this::deleteSolrProductCollectionDocument);
  }

  @Override
  public List<PredefinedAllowedAttributeValueResponse> getAllActiveBrandsByCategoryCodes(List<String> categoryCodes) {
    List<String> brands = this.solrActiveProductCollectionRepository.getAllActiveBrandsByCategoryCodes(categoryCodes);
    List<PredefinedAllowedAttributeValueResponse> predefinedAllowedAttributeValueResponses =
        brands.stream().sorted().map(brand -> getPredefinedAllowedAttributeValueResponse(brand))
            .collect(Collectors.toList());
    return predefinedAllowedAttributeValueResponses;
  }

  @Override
  public void deleteSolrProductCollectionByDocumentId(String documentId) {
    this.solrActiveProductCollectionRepository.deleteSolrProductCollectionByDocumentId(documentId);
  }

  private PredefinedAllowedAttributeValueResponse getPredefinedAllowedAttributeValueResponse(String brandValue) {
    PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse = new PredefinedAllowedAttributeValueResponse();
    predefinedAllowedAttributeValueResponse.setValue(brandValue);
    return predefinedAllowedAttributeValueResponse;
  }

  @Override
  public void deleteSolrProductCollectionDocument(String documentId) {
    try {
      SolrProductCollectionUpdateEvent solrProductCollectionUpdateEvent =
          CommonUtils.getSolrProductCollectionUpdateEvent(documentId);
      kafkaProducer.send(DomainEventName.SOLR_PRODUCT_COLLECTION_UPDATE, documentId, solrProductCollectionUpdateEvent);
    } catch (Exception e) {
      log.error("error while deleting productCollection solr document from solr core: " + "prd_collection. docId: {}",
          documentId, e);
    }
  }
}
