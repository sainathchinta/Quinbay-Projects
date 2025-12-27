package com.gdn.x.productcategorybase.solr.dao.impl;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.regex.Matcher;

import org.apache.commons.lang3.StringUtils;
import org.apache.solr.client.solrj.SolrQuery;
import org.apache.solr.client.solrj.SolrServerException;
import org.apache.solr.client.solrj.impl.CloudSolrClient;
import org.apache.solr.client.solrj.request.QueryRequest;
import org.apache.solr.client.solrj.response.QueryResponse;
import org.apache.solr.common.SolrDocument;
import org.apache.solr.common.SolrDocumentList;
import org.apache.solr.common.SolrInputDocument;
import org.apache.solr.common.params.ModifiableSolrParams;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Repository;
import org.springframework.util.CollectionUtils;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.productcategorybase.Constants;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductItem;
import com.gdn.x.productcategorybase.entity.ProductItemImage;
import com.gdn.x.productcategorybase.solr.dao.SolrProductDao;
import com.gdn.x.productcategorybase.solr.model.AttributeModel;
import com.gdn.x.productcategorybase.solr.model.ProductDocument;
import com.gdn.x.productcategorybase.solr.model.ReIndexType;

/**
 * Created by Kesha on 24/04/16.
 */
@Repository
public class SolrProductDaoImpl implements SolrProductDao {

  @Autowired
  @Qualifier("pcbCollectionClient")
  private CloudSolrClient solrClient;

  private static final Logger LOGGER = LoggerFactory.getLogger(SolrProductDaoImpl.class);
  private static final String COMMAND = "command";
  private static final String FULL_IMPORT_COMMAND = "full-import";
  private static final String DELTA_IMPORT_COMMAND = "delta-import";
  private static final String COMMIT = "commit";
  private static final String IS_COMMIT = "true";
  private static final String DATA_IMPORT_PATH = "/dataimport";
  private static final String NAME_FIELD = "name";
  private static final String PRODUCT_CODE_FIELD = "product_code";
  private static final String ID_FIELD = "id";
  private static final String GENERATED_ITEM_NAMES_FIELD = "generated_item_names";
  private static final String UPC_CODES_FIELD = "upc_codes";
  private static final String SKU_CODES_FIELD = "sku_codes";
  private static final String LOCATION_PATHS_FIELD = "location_paths";
  private static final String DANGEROUS_GOODS_LEVEL_FIELD = "dangerous_goods_levels";
  private static final String SOLR_QUERY = "q";
  private static final String SOLR_QUERY_NAME_PARAM = "name:";
  private static final String SOLR_QUERY_AND_PARAM = " AND ";
  private static final String SOLR_QUERY_CATEGORY_ID_PARAM = "category_id:";

  private SolrInputDocument buildSolrDocument(ProductDocument document) {
    if (StringUtils.isAnyBlank(document.getId(), document.getCategoryId(), document
        .getProductCode())) {
      return null;
    }
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    solrInputDocument.addField("id", document.getId());
    solrInputDocument.addField("name", document.getProductName());
    solrInputDocument.addField("category_id", document.getCategoryId());
    solrInputDocument.addField("product_category_id", document.getProductCategoryId());
    solrInputDocument.addField("product_code", document.getProductCode());
    solrInputDocument.addField("upc_codes", document.getUpcCodes());
    solrInputDocument.addField("updated_date", document.getUpdatedDate());
    if (document.getAttributes() != null) {
      for (AttributeModel model : document.getAttributes()) {
        if (StringUtils.isAnyBlank(model.getName(), model.getValue())) {
          continue;
        }
        String key = model.getName() + "_val";
        if (solrInputDocument.getField(key) == null) {
          solrInputDocument.addField(key, model.getValue());
        }else {
          solrInputDocument.setField(key, solrInputDocument.getField(key).getValue() + "," + model
              .getValue());
        }
      }
    }

    return solrInputDocument;
  }

  @Override
  public boolean fullIndex() {
    try {
      ModifiableSolrParams params = new ModifiableSolrParams();
      params.set(COMMAND, FULL_IMPORT_COMMAND);
      params.set(COMMIT, IS_COMMIT);
      QueryRequest request = new QueryRequest(params);
      request.setPath(DATA_IMPORT_PATH);
      this.solrClient.request(request);
      return true;
    }
    catch (IOException iOException) {
      LOGGER.error("full index failed due to IO exception", iOException);
    }
    catch (SolrServerException solrServerException) {
      LOGGER.error("full index failed due to Solr Server Exception exception", solrServerException);
    }
    return false;
  }

  @Override
  public boolean deltaIndex() {
    try {
      ModifiableSolrParams params = new ModifiableSolrParams();
      params.set(COMMAND, DELTA_IMPORT_COMMAND);
      params.set(COMMIT, IS_COMMIT);
      QueryRequest request = new QueryRequest(params);
      request.setPath(DATA_IMPORT_PATH);
      this.solrClient.request(request);
      return true;
    }
    catch (IOException iOException) {
      LOGGER.error("delta index failed due to IO exception", iOException);
    }
    catch (SolrServerException solrServerException) {
      LOGGER.error("delta index failed due to Solr Server Exception exception", solrServerException);
    }
    return false;
  }

  @Override
  public void deleteAllDocuments() throws IOException, SolrServerException {
    try {
      solrClient.deleteByQuery("*:*");
      solrClient.commit();
    } catch (SolrServerException | IOException e) {
      LOGGER.error("deleting all documents failed for solr while full reindexing job", e);
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, e.getMessage(), e);
    }
  }

  @Override
  public void post(List<ProductDocument> documentList, ReIndexType indexType) {
    LOGGER.info("Reindex : {} , posting {} documents to solr for thread {}", indexType.name(),
        documentList.size(), Thread.currentThread().getName());
    List<SolrInputDocument> solrDocuments = new ArrayList<>();
    for (ProductDocument document : documentList) {
      SolrInputDocument solrDocument = buildSolrDocument(document);
      if (solrDocument != null) {
        solrDocuments.add(solrDocument);
      }
    }
    try {
      if (!CollectionUtils.isEmpty(solrDocuments)) {
        solrClient.add(solrDocuments);
      }

    } catch (SolrServerException | IOException e) {
      LOGGER.error("Exception occurred while posting {} documents to solr", solrDocuments.size
          (), e);
    }
  }

  @Override
  public void commit() throws IOException, SolrServerException {
    solrClient.commit(true, true);
  }

  @Override
  public void deleteDocuments(List<String> deleteList) {
    try {
      if (!CollectionUtils.isEmpty(deleteList)) {
        solrClient.deleteById(deleteList);
      }

    } catch (SolrServerException | IOException e) {
      LOGGER.error("Exception occurred while deleting {} documents to solr for delta reindex",
          deleteList.size(), e);
    }
  }

  @Override
  public List<ProductItem> findProductItemsWithCategoryIdAndGeneratedItemName(
      String generatedItemName, String categoryId, int solrStart, int solrRows) {
    Matcher matcher = Constants.PATTERN_FOR_SPECIAL_CHARACTERS.matcher(generatedItemName);
    generatedItemName = StringUtils.trimToEmpty(matcher.replaceAll(StringUtils.SPACE));
    int pageSizeCounter = solrRows;
    SolrQuery query = new SolrQuery();
    QueryResponse response = new QueryResponse();
    List<ProductItem> dataList = new ArrayList<>();
    StringBuilder solrQuery = new StringBuilder();
    query.set(SOLR_QUERY, String.valueOf(
        solrQuery.append(SOLR_QUERY_NAME_PARAM).append(generatedItemName).append(SOLR_QUERY_AND_PARAM).append(SOLR_QUERY_CATEGORY_ID_PARAM).append(categoryId)));
    query.setStart(solrStart);
    query.setRows(solrRows);
    try {
      response = solrClient.query(query);
    } catch (SolrServerException e) {
      LOGGER.error("Solr Server Error for query : {} ", query, e);
    } catch (IOException e) {
      LOGGER.error("Solr I/O Error for query : {} ", query, e);
    }
    if (Objects.isNull(response) || CollectionUtils.isEmpty(response.getResults())) {
      return dataList;
    }
    SolrDocumentList solrDocuments = response.getResults();
    for (SolrDocument solrDocument : solrDocuments) {
      Product product = getProduct(solrDocument);
      String[] itemNamesList = getStrings(solrDocument,GENERATED_ITEM_NAMES_FIELD);
      String[] upcCodesList = getStrings(solrDocument,UPC_CODES_FIELD);
      String[] skuCodesList = getStrings(solrDocument,SKU_CODES_FIELD);
      String[] locationPathsList = getStrings(solrDocument,LOCATION_PATHS_FIELD);
      Object[] dgl = solrDocument.getFieldValues(DANGEROUS_GOODS_LEVEL_FIELD).toArray();
      Integer[] dglList = Arrays.asList(dgl).toArray(new Integer[0]);

      for (int productItemCounter = 0;
           productItemCounter < upcCodesList.length; productItemCounter++) {
        if (pageSizeCounter == 0)
          break;
        ProductItem productItem = getProductItem(product, itemNamesList[productItemCounter],
            upcCodesList[productItemCounter], skuCodesList[productItemCounter],
            locationPathsList[productItemCounter], dglList[productItemCounter]);
        dataList.add(productItem);
        pageSizeCounter--;
      }
    }
    return dataList;
  }

  /**
   * generate product for productItem
   * @param solrDocument
   * @return Product
   */
  private Product getProduct(SolrDocument solrDocument) {
    Product product = new Product();
    product.setName(String.valueOf(solrDocument.getFieldValue(NAME_FIELD)));
    product.setProductCode(String.valueOf(solrDocument.getFieldValue(PRODUCT_CODE_FIELD)));
    product.setId(String.valueOf(solrDocument.getFieldValue(ID_FIELD)));
    return product;
  }

  /**
   * generate product item using parameters
   * @param product
   * @param generatedItemName
   * @param upcCode
   * @param skuCode
   * @param locationPath
   * @param dangerousGoodsLevel
   * @return ProductItem
   */
  private ProductItem getProductItem(Product product, String generatedItemName, String upcCode,
      String skuCode, String locationPath, Integer dangerousGoodsLevel) {
    ProductItem productItem = new ProductItem();
    ProductItemImage productItemImage = new ProductItemImage();
    List<ProductItemImage> productItemImages = new ArrayList<>();
    productItem.setGeneratedItemName(generatedItemName);
    productItem.setUpcCode(upcCode);
    productItem.setSkuCode(skuCode);
    productItemImage.setLocationPath(locationPath);
    productItemImage.setMainImages(true);
    productItemImages.add(productItemImage);
    productItem.setProductItemImages(productItemImages);
    productItem.setDangerousGoodsLevel(dangerousGoodsLevel);
    productItem.setProduct(product);
    productItem.setActivated(Boolean.TRUE);
    productItem.setViewable(Boolean.TRUE);
    return productItem;
  }

  /**
   * extract productItem attribute values from solr document
   * @param solrDocument
   * @param fieldName
   * @return String[]
   */
  private String[] getStrings(SolrDocument solrDocument, String fieldName) {
    Object[] stringList = solrDocument.getFieldValues(fieldName).toArray();
    String[] strings = Arrays.stream(stringList).toArray(String[]::new);
    return strings;

  }

}
