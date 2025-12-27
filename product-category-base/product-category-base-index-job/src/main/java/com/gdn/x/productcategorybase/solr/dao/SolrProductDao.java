package com.gdn.x.productcategorybase.solr.dao;

import com.gdn.x.productcategorybase.entity.ProductItem;
import com.gdn.x.productcategorybase.solr.model.ProductDocument;
import com.gdn.x.productcategorybase.solr.model.ReIndexType;

import java.util.List;

/**
 * Created by Kesha on 02/05/16.
 */
public interface SolrProductDao {
  /**
   * Delete all documents from solr
   * @throws Exception
   */
  void deleteAllDocuments() throws Exception;

  /**
   * Post selected documents to solr
   * @param documentList
   * @param indexType
   */
  void post(List<ProductDocument> documentList, ReIndexType indexType);

  void commit() throws Exception;

  /**
   * Delete selected documents from solr
   * @param deleteList
   */
  void deleteDocuments(List<String> deleteList);

  /**
   * updates all the products into Solr database
   *
   */
  boolean fullIndex();

  /**
   * Updates products which are modified after last reindex
   *
   */
  boolean deltaIndex();

  /**
   * Filter ProductItem data using generated name and categoryId
   * @param generatedItemName
   * @param categoryId
   * @param solrStart
   * @param solrRows
   */
  List<ProductItem> findProductItemsWithCategoryIdAndGeneratedItemName(String generatedItemName,String categoryId,int solrStart,int solrRows);
}
