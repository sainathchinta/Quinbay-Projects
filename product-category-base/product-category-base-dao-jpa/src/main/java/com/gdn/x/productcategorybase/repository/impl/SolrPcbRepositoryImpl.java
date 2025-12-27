package com.gdn.x.productcategorybase.repository.impl;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.apache.solr.client.solrj.SolrServerException;
import org.apache.solr.client.solrj.impl.CloudSolrClient;
import org.apache.solr.common.SolrInputDocument;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.x.productcategorybase.SolrConstants;
import com.gdn.x.productcategorybase.SolrFieldNames;
import com.gdn.x.productcategorybase.entity.solr.SolrPcbProductModel;
import com.gdn.x.productcategorybase.repository.SolrPcbRepository;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Repository
public class SolrPcbRepositoryImpl implements SolrPcbRepository {

  @Autowired
  @Qualifier("pcbCollectionClient")
  private CloudSolrClient pcbCollectionClient;

  @Override
  public void addProductListToPcbCollection(List<SolrPcbProductModel> solrPcbProductModelList) {
    List<SolrInputDocument> solrInputDocuments = new ArrayList<>();
    List<String> productCodes = new ArrayList<>();
    for (SolrPcbProductModel solrPcbProductModel : solrPcbProductModelList){
      productCodes.add(solrPcbProductModel.getProductCode());
      SolrInputDocument solrInputDocument = new SolrInputDocument();
      solrInputDocument.setField(SolrFieldNames.ID, solrPcbProductModel.getId());
      solrInputDocument.setField(SolrFieldNames.PRODUCT_NAME, solrPcbProductModel.getName());
      solrInputDocument.setField(SolrFieldNames.CATEGORY_ID, solrPcbProductModel.getCategoryId());
      solrInputDocument.setField(SolrFieldNames.PRODUCT_CODE, solrPcbProductModel.getProductCode());
      solrInputDocument.setField(SolrFieldNames.PARENT_CATEGORY_ID, solrPcbProductModel.getParentCategoryId());
      solrInputDocument.setField(SolrFieldNames.UPC_CODES, solrPcbProductModel.getUpcCodes());
      solrInputDocument.setField(SolrFieldNames.SKU_CODES, solrPcbProductModel.getSkuCodes());
      solrInputDocument.setField(SolrFieldNames.GENERATED_ITEM_NAMES, solrPcbProductModel.getGeneratedItemNames());
      solrInputDocument.setField(SolrFieldNames.DANGEROUS_GOODS_LEVELS, solrPcbProductModel.getDangerousGoodsLevels());
      solrInputDocument.setField(SolrFieldNames.IMAGE_LOCATION_PATHS, solrPcbProductModel.getImageLocationPaths());
      solrInputDocument.setField(SolrFieldNames.UPDATED_DATE, new Date());
      solrInputDocuments.add(solrInputDocument);
    }
    try {
      pcbCollectionClient.add(solrInputDocuments);
    } catch (Exception e) {
      log.error("Exception caught while adding documents to pcb collection, productCodes:{}", productCodes, e);
    }
  }

  @Override
  public void deleteProductListFromPcbCollection(List<String> ids) {
    try {
      pcbCollectionClient.deleteById(ids);
    } catch (Exception e) {
      log.error("Exception caught while deleting document from pcb collection, id: {}", ids, e);
    }
  }

  @Override
  public void deleteAllDocumentsFromSolr() throws IOException, SolrServerException {
    pcbCollectionClient.deleteByQuery(SolrConstants.DEFAULT_QUERY);
  }

}
