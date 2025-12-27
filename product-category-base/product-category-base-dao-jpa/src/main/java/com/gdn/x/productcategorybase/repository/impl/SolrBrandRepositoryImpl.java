package com.gdn.x.productcategorybase.repository.impl;

import java.io.IOException;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import org.apache.solr.client.solrj.SolrServerException;
import org.apache.solr.client.solrj.impl.CloudSolrClient;
import org.apache.solr.common.SolrInputDocument;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Repository;

import com.gdn.x.productcategorybase.SolrConstants;
import com.gdn.x.productcategorybase.SolrFieldNames;
import com.gdn.x.productcategorybase.entity.solr.SolrBrandModel;
import com.gdn.x.productcategorybase.repository.SolrBrandRepository;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Repository
public class SolrBrandRepositoryImpl implements SolrBrandRepository {

  @Autowired
  @Qualifier("brandCollectionClient")
  private CloudSolrClient brandCollectionClient;

  @Override
  public void addBrandsToBrandCollectionSolr(List<SolrBrandModel> solrBrandModels) {
    List<SolrInputDocument> solrDocuments =
        solrBrandModels.stream().map(this::generateSolrInputDocumentForBrand).collect(Collectors.toList());
    try {
      this.brandCollectionClient.add(solrDocuments);
    } catch (Exception e) {
      log.error("Exception caught while adding document to brand collection : {}", solrDocuments, e);
    }
  }

  @Override
  public void deleteBrandsFromBrandCollectionSolr(List<String> id) {
    try {
      this.brandCollectionClient.deleteById(id);
    } catch (Exception e) {
      log.error("Exception caught while deleting document to brand collection, id: {}", id, e);
    }
  }

  private SolrInputDocument getSolrInputFieldsToUpdateBrandApprovalStatus(SolrBrandModel model) {
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    solrInputDocument.setField(SolrFieldNames.ID, model.getId());
    solrInputDocument.setField(SolrFieldNames.BUSINESS_PARTNER_CODE, Collections
        .singletonMap(SolrConstants.SET, SolrConstants.NA));
    solrInputDocument.setField(SolrFieldNames.BRAND_APPROVED, Collections.singletonMap(SolrConstants.SET, Boolean.TRUE));
    if (Boolean.TRUE.equals(model.getBrandNameUpdated())) {
      solrInputDocument.setField(SolrFieldNames.BRAND_VALUE,
          Collections.singletonMap(SolrConstants.SET, model.getBrandValue()));
    }
    solrInputDocument.setField(SolrFieldNames.BRAND_CODE, Collections.singletonMap(SolrConstants.SET, model.getBrandCode()));
    return solrInputDocument;
  }

  @Override
  public void updateBrandApprovalStatusListInSolr(List<SolrBrandModel> solrBrandModels) {
    List<SolrInputDocument> solrInputDocuments =
        solrBrandModels.stream().map(this::getSolrInputFieldsToUpdateBrandApprovalStatus).collect(Collectors.toList());
    try {
      this.brandCollectionClient.add(solrInputDocuments);
    } catch (Exception e) {
      log.error("Exception caught while updating document in brand collection, {}", solrBrandModels, e);
    }
  }

  @Override
  public void updateBrandNameInSolr(String id, String brandName, boolean protectedBrand) {
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    solrInputDocument.setField(SolrFieldNames.ID, id);
    solrInputDocument.setField(SolrFieldNames.BRAND_VALUE, Collections.singletonMap(SolrConstants.SET, brandName));
    solrInputDocument.setField(SolrFieldNames.PROTECTED_BRAND,
        Collections.singletonMap(SolrConstants.SET, protectedBrand));
    try {
      this.brandCollectionClient.add(solrInputDocument);
    } catch (SolrServerException | IOException e) {
      log.error("Exception caught while updating brand name in brand collection, brandName: {}", brandName, e);
    }
  }

  @Override
  public void deleteAllDocumentsFromSolr() throws IOException, SolrServerException {
    brandCollectionClient.deleteByQuery(SolrConstants.DEFAULT_QUERY);
  }

  private SolrInputDocument generateSolrInputDocumentForBrand(SolrBrandModel solrBrandModel) {
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    solrInputDocument.setField(SolrFieldNames.ID, solrBrandModel.getId());
    solrInputDocument.setField(SolrFieldNames.BRAND_CODE, solrBrandModel.getBrandCode());
    solrInputDocument.setField(SolrFieldNames.BRAND_APPROVED, solrBrandModel.isBrandApproved());
    solrInputDocument.setField(SolrFieldNames.BRAND_VALUE, solrBrandModel.getBrandValue());
    solrInputDocument.setField(SolrFieldNames.BUSINESS_PARTNER_CODE, solrBrandModel.getBusinessPartnerCode());
    solrInputDocument.setField(SolrFieldNames.UPDATED_DATE, solrBrandModel.getUpdatedDate());
    solrInputDocument.setField(SolrFieldNames.PROTECTED_BRAND, solrBrandModel.isProtectedBrand());
    return solrInputDocument;
  }
}
