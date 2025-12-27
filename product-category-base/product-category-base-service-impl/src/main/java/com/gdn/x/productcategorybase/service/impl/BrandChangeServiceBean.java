package com.gdn.x.productcategorybase.service.impl;

import java.io.IOException;
import java.util.Collections;
import java.util.Objects;

import org.apache.solr.client.solrj.SolrServerException;
import org.apache.solr.client.solrj.impl.CloudSolrClient;
import org.apache.solr.common.SolrException;
import org.apache.solr.common.SolrInputDocument;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.x.productcategorybase.SolrConstants;
import com.gdn.x.productcategorybase.SolrFieldNames;
import com.gdn.x.productcategorybase.entity.brand.Brand;
import com.gdn.x.productcategorybase.repository.brand.BrandRepository;
import com.gdn.x.productcategorybase.service.BrandChangeService;
import com.gdn.x.productcategorybase.service.brand.BrandServiceBean;

@Service
@Transactional(readOnly = true)
public class BrandChangeServiceBean implements BrandChangeService {

  @Autowired
  @Qualifier("brandCollectionClient")
  private CloudSolrClient solrClientBrand;

  @Autowired
  private BrandRepository brandRepository;

  private static final Logger LOGGER = LoggerFactory.getLogger(BrandServiceBean.class);
  private static final String ID = "id";
  private static final String BRAND_CODE = "brand_code";
  private static final String BRAND_NAME = "brand_value";
  private static final String UPDATED_DATE = "updated_date";
  private static final String PROTECTED_BRAND = "protected_brand";


  @Async
  @Override
  public void createSolrDocumentForBrandCollection(Brand brand) throws Exception {
    SolrInputDocument document = new SolrInputDocument();
    try {
      solrClientBrand.add(generateSolrDocument(document, brand));
      solrClientBrand.commit();
    } catch (SolrServerException | IOException solrOrIOException) {
      LOGGER.error("Brand document creation failed due to SolrServerException or IOException for Brand code:{} ",
          brand.getBrandCode(), solrOrIOException);
    } catch (Exception exception) {
      LOGGER.error("Unknown exception caught while creating brand document in solr for brand code: {}",
          brand.getBrandCode(), exception);
    }
  }

  private SolrInputDocument generateSolrDocument(SolrInputDocument document, Brand brand) {
    document.addField(ID, brand.getId());
    document.addField(BRAND_CODE, brand.getBrandCode());
    document.addField(BRAND_NAME, brand.getBrandName());
    document.addField(UPDATED_DATE, brand.getUpdatedDate());
    document.addField(PROTECTED_BRAND, brand.isProtectedBrand());
    return document;
  }

  @Async
  @Override
  public void deleteSolrDocumentFromBrandCollection(String brandCode, String storeId) throws Exception {
    Brand brand = brandRepository.findFirstByStoreIdAndBrandCodeAndMarkForDeleteTrue(storeId, brandCode);
    try {
      if (Objects.nonNull(brand)) {
        solrClientBrand.deleteById(brand.getId());
        solrClientBrand.commit();
      } else {
        LOGGER.info("Attempt to delete the record which is not been deleted from predefined allowed attribute value , "
            + "brand_code: {}", brandCode);
      }
    } catch (SolrServerException | IOException solrOrIOException) {
      LOGGER.error(
          "Deleting document from solr brand collection failed because of SolrServer or IO exception, brand_code : {}",
          brandCode, solrOrIOException);
    } catch (Exception exception) {
      LOGGER.error("Unknown exception caught while deleting document from solr, brand_code : {}", brandCode, exception);
    }
  }

  @Override
  public void updateProtectedBrand(String id, boolean protectedBrand) {
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    solrInputDocument.setField(SolrFieldNames.ID, id);
    solrInputDocument.setField(SolrFieldNames.PROTECTED_BRAND,
        Collections.singletonMap(SolrConstants.SET, protectedBrand));
    try {
      solrClientBrand.add(solrInputDocument);
    } catch (SolrServerException | SolrException | IOException e) {
      LOGGER.error("Brand document update failed due to SolrServerException or IOException for Brand id:{} ", id,
          e);
    }
  }
}
