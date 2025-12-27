package com.gdn.x.productcategorybase.service.impl;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.time.DateUtils;
import com.gdn.common.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.x.productcategorybase.Constants;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductArchive;
import com.gdn.x.productcategorybase.entity.ProductAttributeArchive;
import com.gdn.x.productcategorybase.entity.ProductAttributeValueArchive;
import com.gdn.x.productcategorybase.entity.ProductCategoryArchive;
import com.gdn.x.productcategorybase.entity.ProductImageArchive;
import com.gdn.x.productcategorybase.entity.ProductItemArchive;
import com.gdn.x.productcategorybase.entity.ProductItemAttributeValueArchive;
import com.gdn.x.productcategorybase.entity.ProductItemImageArchive;
import com.gdn.x.productcategorybase.repository.ProductArchiveRepository;
import com.gdn.x.productcategorybase.repository.ProductAttributeArchiveRepository;
import com.gdn.x.productcategorybase.repository.ProductAttributeValueArchiveRepository;
import com.gdn.x.productcategorybase.repository.ProductCategoryArchiveRepository;
import com.gdn.x.productcategorybase.repository.ProductImageArchiveRepository;
import com.gdn.x.productcategorybase.repository.ProductItemArchiveRepository;
import com.gdn.x.productcategorybase.repository.ProductItemAttributeValueArchiveRepository;
import com.gdn.x.productcategorybase.repository.ProductItemImageArchiveRepository;
import com.gdn.x.productcategorybase.service.ProductArchivalService;
import com.gdn.x.productcategorybase.service.ProductDeletionService;
import com.gdn.x.productcategorybase.service.ProductService;
import com.gdn.x.productcategorybase.service.SystemParameterService;
import com.gdn.x.productcategorybase.util.ConverterUtil;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
@Transactional(readOnly = false)
public class ProductArchivalServiceImpl implements ProductArchivalService {

  @Autowired
  private ProductArchiveRepository productArchiveRepository;

  @Autowired
  private ProductItemArchiveRepository productItemArchiveRepository;

  @Autowired
  private ProductAttributeArchiveRepository productAttributeArchiveRepository;

  @Autowired
  private ProductAttributeValueArchiveRepository productAttributeValueArchiveRepository;

  @Autowired
  private ProductItemAttributeValueArchiveRepository productItemAttributeValueArchiveRepository;

  @Autowired
  private ProductCategoryArchiveRepository productCategoryArchiveRepository;

  @Autowired
  private ProductImageArchiveRepository productImageArchiveRepository;

  @Autowired
  private ProductItemImageArchiveRepository productItemImageArchiveRepository;

  @Autowired
  private ProductDeletionService productDeletionService;

  @Autowired
  private ProductService productService;

  @Autowired
  private SystemParameterService systemParameterService;

  @Override
  @Transactional(readOnly = false, propagation = Propagation.REQUIRES_NEW, rollbackFor = Exception.class)
  public void copyProductDetailsToArchiveTablesAndHardDeleteProduct(Product product) throws Exception {
    log.info("Archiving product {} ", product.getProductCode());

    //copy pcc_product to pcc_product_archive details
    ProductArchive productArchive = new ProductArchive();
    BeanUtils.copyProperties(product, productArchive);
    productArchive.setProductId(product.getId());
    productArchiveRepository.save(productArchive);

    //copy pcc_product_images
    if (CollectionUtils.isNotEmpty(product.getProductCategories())) {
      List<ProductImageArchive> productImageArchiveList = new ArrayList<>();
      ConverterUtil.setProductImageArchiveList(product.getProductImages(), productImageArchiveList);
      productImageArchiveRepository.saveAll(productImageArchiveList);
    }

    //copy pcc_product_category
    if (CollectionUtils.isNotEmpty(product.getProductImages())) {
      List<ProductCategoryArchive> productCategoryArchiveList = new ArrayList<>();
      ConverterUtil.setProductCategoryArchiveList(product.getProductCategories(), productCategoryArchiveList);
      productCategoryArchiveRepository.saveAll(productCategoryArchiveList);
    }

    //copy pcc_product_attribute
    if (CollectionUtils.isNotEmpty(product.getProductAttributes())) {
      List<ProductAttributeArchive> productAttributeArchiveList = new ArrayList<>();
      List<ProductAttributeValueArchive> productAttributeValueArchiveList = new ArrayList<>();
      ConverterUtil.setProductAttributeArchiveList(product.getProductAttributes(), productAttributeArchiveList,
          productAttributeValueArchiveList);

      productAttributeArchiveRepository.saveAll(productAttributeArchiveList);

      if (CollectionUtils.isNotEmpty(productAttributeValueArchiveList)) {
        productAttributeValueArchiveRepository.saveAll(productAttributeValueArchiveList);
      }
    }

    //copy pcc_product_item to pcc_produc_item_archive details
    if (CollectionUtils.isNotEmpty(product.getProductItems())) {
      List<ProductItemArchive> productItemArchiveList = new ArrayList<>();
      List<ProductItemAttributeValueArchive> productItemAttributeValueArchiveList = new ArrayList<>();
      List<ProductItemImageArchive> productItemImageArchiveList = new ArrayList<>();
      ConverterUtil.setProductItemArchiveList(product.getProductItems(), productItemArchiveList, productItemAttributeValueArchiveList,
          productItemImageArchiveList);

      productItemArchiveRepository.saveAll(productItemArchiveList);

      if (CollectionUtils.isNotEmpty(productItemAttributeValueArchiveList)) {
        productItemAttributeValueArchiveRepository.saveAll(productItemAttributeValueArchiveList);
      }

      if (CollectionUtils.isNotEmpty(productItemImageArchiveList)) {
        productItemImageArchiveRepository.saveAll(productItemImageArchiveList);
      }
    }

    log.info("Hard deleting product {} ", product.getProductCode());
    //hard delete product
    productDeletionService.deleteProductData(product);

    log.info("Clearing product cache product {} ", product.getProductCode());
    //clear product cache
    productService.evictAllProductDetailCache(product.getStoreId(), product);
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void deleteDataFromArchivalTable(String storeId) throws Exception {
    int batchSize = Integer.parseInt(
        systemParameterService.findByStoreIdAndVariable(storeId, Constants.BATCH_SIZE_TO_DELETE_ARCHIVED_PRODUCTS)
            .getValue());
    if (batchSize != 0) {
      int dateThreshold = Integer.parseInt(systemParameterService.findByStoreIdAndVariable(storeId,
          Constants.DAYS_THRESHOLD_FOR_ARCHIVED_PRODUCT_DELETION).getValue());
      Date dateFilter = DateUtils.addDays(new Date(), -dateThreshold);
      List<String> productIds = productArchiveRepository.findIdByCreatedDateLessThan(dateFilter, batchSize);
      log.info("Deleting archive product data for productIds : {} ", productIds);
      if (CollectionUtils.isNotEmpty(productIds)) {
        productAttributeValueArchiveRepository.deleteByProductIdIn(productIds);
        productItemAttributeValueArchiveRepository.deleteByProductIdIn(productIds);
        productItemImageArchiveRepository.deleteByProductIdIn(productIds);
        productImageArchiveRepository.deleteByProductIdIn(productIds);
        productAttributeArchiveRepository.deleteByProductIdIn(productIds);
        productCategoryArchiveRepository.deleteByProductIdIn(productIds);
        productItemArchiveRepository.deleteByProductIdIn(productIds);
        productArchiveRepository.deleteByProductIdIn(productIds);
      }
      log.info("Deletion of archive product data completed for productIds : {} ", productIds);
    }
  }
}
