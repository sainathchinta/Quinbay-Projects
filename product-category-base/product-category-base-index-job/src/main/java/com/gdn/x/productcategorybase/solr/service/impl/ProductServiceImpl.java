package com.gdn.x.productcategorybase.solr.service.impl;

import com.gdn.x.productcategorybase.solr.dao.ProductDao;
import com.gdn.x.productcategorybase.solr.dao.ProductDeltaDao;
import com.gdn.x.productcategorybase.solr.dao.SolrProductDao;
import com.gdn.x.productcategorybase.solr.model.ActionType;
import com.gdn.x.productcategorybase.solr.model.AttributeModel;
import com.gdn.x.productcategorybase.solr.model.DeltaProduct;
import com.gdn.x.productcategorybase.solr.model.ProductDocument;
import com.gdn.x.productcategorybase.solr.model.ProductModel;
import com.gdn.x.productcategorybase.solr.model.ReIndexType;
import com.gdn.x.productcategorybase.solr.service.ProductService;
import com.gdn.x.productcategorybase.solr.util.ListSplitter;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Created by Kesha on 24/04/16.
 */
@Service
public class ProductServiceImpl implements ProductService {
  @Autowired
  private ProductDao productDao;

  @Autowired
  private SolrProductDao solrProductDao;

  @Autowired
  private ProductDeltaDao productDeltaDao;

  private static final Logger LOGGER = LoggerFactory.getLogger(ProductServiceImpl.class);

  @Override
  public List<ProductModel> getAllProducts() {
    return productDao.getAllProducts();
  }

  @Override
  public void getDataAndPostDocumentsToSolr(List<ProductModel> productModelList,
      Map<String, String> categoryToParentMap, ReIndexType indexType) throws Exception {
    LOGGER.info("Thread {} started solr processing for {} products", Thread.currentThread()
        .getName(), productModelList.size());
    ListSplitter<ProductModel> splitter = new ListSplitter<>();
    for (List<ProductModel> productModelChunks : splitter.splitByMaxListSize(productModelList,
        1000)) {
      Set<String> productIdList = getProductIds(productModelChunks);
      Map<String, List<AttributeModel>> productToAttributes = productDao.getProductAttributes
          (productIdList);
      Map<String, String> productToCategoryMap = productDao.getProductCategories(productIdList);
      Map<String, Set<String>> upcCodeList = productDao.getProductToUPCCodesMap(productIdList);
      List<ProductDocument> documentList = getProductDocuments(productModelChunks,
          categoryToParentMap, productToAttributes, productToCategoryMap, upcCodeList);
      if (!CollectionUtils.isEmpty(documentList)) {
        solrProductDao.post(documentList, indexType);
        solrProductDao.commit();
      }
    }
  }

  @Override
  public Map<String, String> getCategoryToParentMap() {
    Map<String, String> categoryToParentMap = productDao.getAllCategoryMapping();
    return getFinalParentCategoryMapping(categoryToParentMap);
  }

  @Override
  public void
  deltaUpdate(List<DeltaProduct> deltaProductList, Map<String, String>
      productCategoriesMapping) throws Exception {
    LOGGER.debug("Thread {} start processing {} delta products", Thread.currentThread().getName()
        , deltaProductList.size());
    Map<ActionType, List<ProductModel>> actionTypeListMap = filterDeletedProducts(deltaProductList);
    if (!CollectionUtils.isEmpty(actionTypeListMap.get(ActionType.UPDATE))) {
      getDataAndPostDocumentsToSolr(actionTypeListMap.get(ActionType.UPDATE),
          productCategoriesMapping, ReIndexType.DELTA);
    }
    if (!CollectionUtils.isEmpty(actionTypeListMap.get(ActionType.DELETE))) {
      Set<String> productIds = getProductIds(actionTypeListMap.get(ActionType.DELETE));
      solrProductDao.deleteDocuments(new ArrayList<>(productIds));
      solrProductDao.commit();
    }
  }

  @Override
  public Set<DeltaProduct> fetchUpdatedProducts() {
    List<DeltaProduct> deltaProductsDb = productDeltaDao.fetchUpdatedProducts();
    //filtering duplicate products
    Set<DeltaProduct> deltaProducts = new HashSet<>(deltaProductsDb);
    if (!CollectionUtils.isEmpty(deltaProducts)) {
      List<String> deltaProductIdList = getProductIdList(deltaProducts);
      productDeltaDao.updateElementStateToProcessing(deltaProductIdList);
    }
    return deltaProducts;
  }

  @Override
  public void deleteProcessedProducts() {
    productDeltaDao.deleteProcessedElements();
  }

  @Override
  public void updateAllDetlaProductsToProcess() {
    productDeltaDao.updateAllElementsToProcessing();
  }

  @Override
  public boolean fullIndex() {
    return solrProductDao.fullIndex();

  }

  @Override
  public boolean deltaIndex() {
    return solrProductDao.deltaIndex();
  }

  private List<String> getProductIdList(Set<DeltaProduct> deltaProducts) {
    List<String> productItemIds = new ArrayList<>();
    for (DeltaProduct product : deltaProducts) {
      productItemIds.add(product.getId());
    }
    return productItemIds;
  }

  private Map<ActionType, List<ProductModel>> filterDeletedProducts(List<DeltaProduct>
      deltaProductList) {
    List<ProductModel> deletedList = new ArrayList<>();
    List<ProductModel> productsToUpdate = new ArrayList<>();
    for (DeltaProduct product : deltaProductList) {
      ProductModel model = new ProductModel(product.getId(), product.getName(), product
          .getProductCode(), product.getUpdatedDate());
      if (ActionType.DELETE.equals(product.getActionType())) {
        deletedList.add(model);
      } else {
        productsToUpdate.add(model);
      }
    }
    Map<ActionType, List<ProductModel>> actionTypeListMap = new HashMap<>();
    actionTypeListMap.put(ActionType.DELETE, deletedList);
    actionTypeListMap.put(ActionType.UPDATE, productsToUpdate);
    return actionTypeListMap;
  }

  private Set<String> getProductIds(List<ProductModel> modelList) {
    Set<String> productIdSet = new HashSet<>();
    for (ProductModel model : modelList) {
      productIdSet.add(model.getId());
    }
    return productIdSet;
  }


  private List<ProductDocument> getProductDocuments(List<ProductModel> productList,
      Map<String, String> categoryToFinalParentMap, Map<String, List<AttributeModel>>
      productAttributes, Map<String, String> productCategoryMap, Map<String, Set<String>>
      upcCodeList) {
    List<ProductDocument> documentList = new ArrayList<>();
    for (ProductModel productItem : productList) {
      if (StringUtils.isAnyBlank(productItem.getId(), productItem.getProductCode(), productItem
          .getName(), productCategoryMap.get(productItem.getId()))) {
        continue;
      }
      ProductDocument productDoc = new ProductDocument();
      productDoc.setId(productItem.getId());
      productDoc.setProductCode(productItem.getProductCode());
      productDoc.setProductName(productItem.getName());
      String categoryId = productCategoryMap.get(productItem.getId());
      String finalParentCategory = categoryToFinalParentMap.get(categoryId);
      productDoc.setCategoryId(finalParentCategory);
      productDoc.setProductCategoryId(categoryId);
      productDoc.setAttributes(productAttributes.get(productItem.getId()));
      productDoc.setUpcCodes(upcCodeList.get(productItem.getId()));
      productDoc.setUpdatedDate(productItem.getUpdatedDate());
      documentList.add(productDoc);
    }
    return documentList;
  }

  private Map<String, String> getFinalParentCategoryMapping(Map<String, String>
      categoryToParentMap) {
    Map<String, String> categoryToFinalParentMapping = new HashMap<>();
    for (String key : categoryToParentMap.keySet()) {
      categoryToFinalParentMapping.put(key, getParent(key, categoryToParentMap));
    }
    return categoryToFinalParentMapping;
  }

  private String getParent(String key, Map<String, String> categoryToParentMap) {
    if (categoryToParentMap.get(key) == null) {
      return key;
    } else {
      return getParent(categoryToParentMap.get(key), categoryToParentMap);
    }
  }

  public void setProductDao(ProductDao productDao) {
    this.productDao = productDao;
  }

  public void setSolrProductDao(SolrProductDao solrProductDao) {
    this.solrProductDao = solrProductDao;
  }

  public void setProductDeltaDao(ProductDeltaDao productDeltaDao) {
    this.productDeltaDao = productDeltaDao;
  }
}
