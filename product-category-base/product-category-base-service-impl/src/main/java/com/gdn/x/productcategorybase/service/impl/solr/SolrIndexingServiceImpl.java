package com.gdn.x.productcategorybase.service.impl.solr;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import com.gdn.x.productcategorybase.ApplicationConfigPropertyNames;
import com.gdn.x.productcategorybase.domain.event.model.SolrAddBatchPcbProductDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.SolrAddBrandListDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.SolrAddPcbProductDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.SolrDeleteBatchPcbProductDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.SolrDeleteBrandDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.SolrDeletePcbProductDomainEventModel;
import com.gdn.x.productcategorybase.entity.ApplicationConfigProperties;
import com.gdn.x.productcategorybase.entity.Category;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductCategory;
import com.gdn.x.productcategorybase.entity.ProductItem;
import com.gdn.x.productcategorybase.entity.ProductItemImage;
import com.gdn.x.productcategorybase.entity.brand.Brand;
import com.gdn.x.productcategorybase.entity.brand.BrandWip;
import com.gdn.x.productcategorybase.entity.brand.BrandWipState;
import com.gdn.x.productcategorybase.entity.solr.SolrBrandModel;
import com.gdn.x.productcategorybase.repository.SolrBrandRepository;
import com.gdn.x.productcategorybase.repository.SolrPcbRepository;
import com.gdn.x.productcategorybase.repository.brand.BrandRepository;
import com.gdn.x.productcategorybase.repository.brand.BrandWipRepository;
import com.gdn.x.productcategorybase.service.ApplicationConfigPropertiesService;
import com.gdn.x.productcategorybase.service.CategoryService;
import com.gdn.x.productcategorybase.service.CategoryServiceWrapper;
import com.gdn.x.productcategorybase.service.DomainEventPublisherService;
import com.gdn.x.productcategorybase.service.ProductService;
import com.gdn.x.productcategorybase.service.solr.SolrIndexingService;
import com.gdn.x.productcategorybase.util.ConverterUtil;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class SolrIndexingServiceImpl implements SolrIndexingService {

  private static final String DEFAULT_USERANME = "system";

  private static final Pageable PAGEABLE = PageRequest.of(0, 100);

  @Value("${solr.pcb.collection.batch.size}")
  private int pcbCollectionReindexBatchSize;

  @Autowired
  private BrandRepository brandRepository;

  @Autowired
  private BrandWipRepository brandWipRepository;

  @Autowired
  private DomainEventPublisherService domainEventPublisherService;

  @Autowired
  private ApplicationConfigPropertiesService applicationConfigPropertiesService;

  @Autowired
  private CategoryService categoryService;

  @Autowired
  private CategoryServiceWrapper categoryServiceWrapper;

  @Autowired
  private ProductService productService;

  @Autowired
  private SolrBrandRepository solrBrandRepository;

  @Autowired
  private SolrPcbRepository solrPcbRepository;

  public void setPcbCollectionReindexBatchSize(int pcbCollectionReindexBatchSize) {
    this.pcbCollectionReindexBatchSize = pcbCollectionReindexBatchSize;
  }

  @Async
  @Override
  public void fullReindexBrandCollection(String storeId) throws Exception {
    this.solrBrandRepository.deleteAllDocumentsFromSolr();
    Pageable activeBrandsPageable = PAGEABLE;
    Pageable brandWipPageable = PAGEABLE;
    Page<Brand> brands;
    do {
      brands = this.brandRepository.findByStoreIdAndMarkForDeleteFalse(storeId, activeBrandsPageable);
      publishEventsToAddActiveBrandToSolr(brands.getContent());
      activeBrandsPageable = brands.nextPageable();
    } while (brands.hasNext());
    Page<BrandWip> brandWips;
    do {
      brandWips = this.brandWipRepository
          .findByStoreIdAndStateAndMarkForDeleteFalse(storeId, BrandWipState.DRAFT, brandWipPageable);
      publishEventsToAddBrandWipToSolr(brandWips.getContent());
      brandWipPageable = brandWips.nextPageable();
    } while (brandWips.hasNext());
    updateApplicationConfigProperties(storeId,
        ApplicationConfigPropertyNames.SOLR_BRAND_COLLECTION_LAST_DELTA_RUN_TIME);
  }

  private void updateApplicationConfigProperties(String storeId, String deltaRunTime) {
    ApplicationConfigProperties applicationConfigProperties = applicationConfigPropertiesService
        .findByStoreIdAndPropertyNameAndMarkForDeleteFalse(storeId, deltaRunTime);
    applicationConfigProperties.setValue(String.valueOf(new Date().getTime()));
    applicationConfigProperties.setUpdatedBy(DEFAULT_USERANME);
    applicationConfigProperties.setUpdatedDate(new Date());
    applicationConfigProperties.setMarkForDelete(false);
    applicationConfigPropertiesService.save(applicationConfigProperties);
  }

  @Async
  @Override
  public void partialReindexBrandCollection(String storeId) throws Exception {
    ApplicationConfigProperties applicationConfigProperties = this.applicationConfigPropertiesService
        .findByStoreIdAndPropertyNameAndMarkForDeleteFalse(storeId,
            ApplicationConfigPropertyNames.SOLR_BRAND_COLLECTION_LAST_DELTA_RUN_TIME);
    Date startDate = new Date(Long.parseLong(applicationConfigProperties.getValue()));
    Date currentDate = new Date();
    Pageable pageable = PAGEABLE;
    partialReindexActiveBrands(storeId, startDate, currentDate, pageable);
    pageable = PAGEABLE;
    partialReindexBrandWips(storeId, startDate, currentDate, pageable);
    applicationConfigProperties.setValue(String.valueOf(currentDate.getTime()));
    applicationConfigProperties.setUpdatedBy(DEFAULT_USERANME);
    applicationConfigProperties.setUpdatedDate(new Date());
    applicationConfigProperties.setMarkForDelete(false);
    applicationConfigPropertiesService.save(applicationConfigProperties);
  }

  @Override
  public void reindexBrandCollectionByBrandRequestCode(String storeId, String brandRequestCode) throws Exception {
    BrandWip brandWip = this.brandWipRepository.findByStoreIdAndBrandRequestCode(storeId, brandRequestCode);
    processUpdatedBrandWips(Collections.singletonList(brandWip));
  }

  @Async
  @Override
  public void fullReindexPCBCollection(String storeId) {
    try {
      solrPcbRepository.deleteAllDocumentsFromSolr();
      List<Category> parentCategories = categoryService.findAllParentCategories(storeId);
      Map<String, String> categoryCodeAndCategoryIdMapForParentCategories =
          parentCategories.stream().collect(Collectors.toMap(Category::getCategoryCode, Category::getId));
      Page<Product> products;
      Pageable pageable = PAGEABLE;
      List<Product> productList = new ArrayList<>();
      do {
        products = productService.findByViewableAndActivatedWithItemsInitialized(storeId,
            true, true, pageable);
        for (Product product : products) {
          if(productList.size() > pcbCollectionReindexBatchSize){
            publishSolrAddPcbProductBatchEvent(productList, categoryCodeAndCategoryIdMapForParentCategories);
            productList.clear();
          }
          productList.add(product);
        }
        pageable = products.nextPageable();
      } while (products.hasNext());
      if(CollectionUtils.isNotEmpty(productList)){
        publishSolrAddPcbProductBatchEvent(productList, categoryCodeAndCategoryIdMapForParentCategories);
      }
      updateApplicationConfigProperties(storeId, ApplicationConfigPropertyNames.SOLR_PCB_COLLECTION_LAST_DELTA_RUN_TIME);
    } catch (Exception ex){
      log.error("Exception while running Pcb collection fullReindex " , ex);
    }
  }

  private void publishSolrAddPcbProductBatchEvent(List<Product> productList,
      Map<String, String> categoryCodeAndCategoryIdMapForParentCategories) throws Exception {
    SolrAddBatchPcbProductDomainEventModel solrAddBatchPcbProductDomainEventModel =
        new SolrAddBatchPcbProductDomainEventModel();
    solrAddBatchPcbProductDomainEventModel.setProductDomainEventModelList(new ArrayList<>());
    for (Product product : productList) {
      addProductToSolrBatchProductList(product, categoryCodeAndCategoryIdMapForParentCategories,
          solrAddBatchPcbProductDomainEventModel);
    }
    domainEventPublisherService.publishSolrAddBatchPcbProductEvent(solrAddBatchPcbProductDomainEventModel);
  }

  private void addProductToSolrBatchProductList(Product product,
      Map<String, String> categoryCodeAndCategoryIdMapForParentCategories,
      SolrAddBatchPcbProductDomainEventModel solrAddBatchPcbProductDomainEventModel) throws Exception {
    for (ProductCategory productCategory: product.getProductCategories()) {
      if(!productCategory.isMarkForDelete()) {
        String parentCategoryCode = categoryServiceWrapper.getFinalParentCategoryCached(
            product.getStoreId(), productCategory.getCategory().getId());
        List<String> upcCodes = new ArrayList<>();
        List<String> skuCodes = new ArrayList<>();
        List<String> generatedItemNames = new ArrayList<>();
        List<Integer> dangerousGoodsLevels = new ArrayList<>();
        List<String> locationPaths = new ArrayList<>();

        setProductItemsDataInSolrEvent(product, upcCodes, skuCodes, generatedItemNames,
            dangerousGoodsLevels, locationPaths);

        SolrAddPcbProductDomainEventModel solrAddPcbProductDomainEventModel =
            SolrAddPcbProductDomainEventModel.builder().id(product.getId()).name(product.getName())
                .productCode(product.getProductCode()).categoryId(productCategory.getCategory().getId())
                .parentCategoryId(categoryCodeAndCategoryIdMapForParentCategories.get(parentCategoryCode))
                .upcCodes(upcCodes).skuCodes(skuCodes)
                .generatedItemNames(generatedItemNames).dangerousGoodsLevels(dangerousGoodsLevels)
                .imageLocationPaths(locationPaths).build();

        solrAddBatchPcbProductDomainEventModel.getProductDomainEventModelList()
            .add(solrAddPcbProductDomainEventModel);
      } else {
        log.debug("product category is marked for delete {}", productCategory);
      }
    }
  }

  @Async
  @Override
  public void deltaReindexPCBCollection(String storeId) throws Exception {
    ApplicationConfigProperties applicationConfigProperties = applicationConfigPropertiesService
        .findByStoreIdAndPropertyNameAndMarkForDeleteFalse(storeId,
            ApplicationConfigPropertyNames.SOLR_PCB_COLLECTION_LAST_DELTA_RUN_TIME);
    List<Category> parentCategories = categoryService.findAllParentCategories(storeId);
    Map<String, String> categoryCodeAndCategoryIdMapForParentCategories =
        parentCategories.stream().collect(Collectors.toMap(Category::getCategoryCode, Category::getId));
    Page<Product> products;
    Date startDate = new Date(Long.parseLong(applicationConfigProperties.getValue()));
    Date endDate = new Date();
    Pageable pageable = PAGEABLE;
    List<Product> solrAddProduct = new ArrayList<>();
    List<Product> solrDeleteProduct = new ArrayList<>();
    do {
      products = productService.findProductsByStoreIdAndUpdatedDateBetweenWithInitialization(storeId,
          startDate, endDate, pageable);
      for (Product product: products) {
        if(product.isMarkForDelete() || !product.isViewable() || !product.isActivated()) {
          solrDeleteProduct.add(product);
        } else {
          solrAddProduct.add(product);
        }
        if (solrAddProduct.size() > this.pcbCollectionReindexBatchSize){
          publishSolrAddPcbProductBatchEvent(solrAddProduct, categoryCodeAndCategoryIdMapForParentCategories);
          solrAddProduct.clear();
        }
        if (solrDeleteProduct.size() > this.pcbCollectionReindexBatchSize){
          publishSolrDeletePcbProductBatchEvent(solrDeleteProduct);
          solrDeleteProduct.clear();
        }
      }
      pageable = products.nextPageable();
    } while (products.hasNext());
    if (CollectionUtils.isNotEmpty(solrAddProduct)) {
      publishSolrAddPcbProductBatchEvent(solrAddProduct, categoryCodeAndCategoryIdMapForParentCategories);
    }
    if (CollectionUtils.isNotEmpty(solrDeleteProduct)) {
      publishSolrDeletePcbProductBatchEvent(solrDeleteProduct);
    }
    applicationConfigProperties.setValue(String.valueOf(endDate.getTime()));
    applicationConfigProperties.setUpdatedBy(DEFAULT_USERANME);
    applicationConfigProperties.setUpdatedDate(new Date());
    applicationConfigProperties.setMarkForDelete(false);
    applicationConfigPropertiesService.save(applicationConfigProperties);
  }

  @Override
  public void deltaReindexPCBCollectionByProductCode(String storeId, String productCode) throws Exception {
    Product product = productService.getProductWithProductCategoriesAndItemsByStoreIdAndProductCodes(storeId,
        Collections.singletonList(productCode)).get(0);
    List<Category> parentCategories = categoryService.findAllParentCategories(storeId);
    Map<String, String> categoryCodeAndCategoryIdMapForParentCategories =
        parentCategories.stream().collect(Collectors.toMap(Category::getCategoryCode, Category::getId));
    List<Product> solrAddProduct = new ArrayList<>();
    List<Product> solrDeleteProduct = new ArrayList<>();
    if (product.isMarkForDelete() || !product.isViewable() || !product.isActivated()) {
      solrDeleteProduct.add(product);
    } else {
      solrAddProduct.add(product);
    }
    if (CollectionUtils.isNotEmpty(solrAddProduct)) {
      publishSolrAddPcbProductBatchEvent(solrAddProduct, categoryCodeAndCategoryIdMapForParentCategories);
    }
    if (CollectionUtils.isNotEmpty(solrDeleteProduct)) {
      publishSolrDeletePcbProductBatchEvent(solrDeleteProduct);
    }
  }

  @Async
  @Override
  public void categoryBasedReindexPCBCollection(String storeId, String categoryCode) throws Exception {
    Category category = categoryService.getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(storeId, categoryCode);
    log.warn("re-index for product collection for category {} : {}", categoryCode, category);

    if (Objects.nonNull(category)) {
      Map<String, String> categoryCodeAndCategoryIdMap =
        Collections.singletonMap(category.getCategoryCode(), category.getId());

      Page<Product> products;
      final List<Product> productList = new ArrayList<>();
      Pageable pageable = PAGEABLE;
      do {
        products = productService.findByCategoryIdWithItemsInitialized(storeId, category.getId(), pageable);
        log.debug("products for given category {} {}", products, pageable);

        if (Objects.nonNull(products)) {
          products.getContent()
            .stream()
            .filter(Product::isActivated)
            .filter(Product::isViewable)
            .filter(product -> !product.isReviewPending())
            .collect(Collectors.toCollection(() -> productList));

          if (productList.size() > pcbCollectionReindexBatchSize) {
            publishSolrAddPcbProductBatchEvent(productList, categoryCodeAndCategoryIdMap);
            productList.clear();
          }
          pageable = products.nextPageable();
        }
      } while (Objects.nonNull(products) && products.hasNext());

      if (CollectionUtils.isNotEmpty(productList)) {
        publishSolrAddPcbProductBatchEvent(productList, categoryCodeAndCategoryIdMap);
      }
    }
  }

  private void publishSolrDeletePcbProductBatchEvent(List<Product> solrDeleteProducts) {
    SolrDeleteBatchPcbProductDomainEventModel solrDeleteBatchPcbProductDomainEventModel =
        new SolrDeleteBatchPcbProductDomainEventModel();
    solrDeleteBatchPcbProductDomainEventModel.setSolrDeletePcbProductDomainEventModels(
        solrDeleteProducts.stream().map(solrDeleteProduct -> {
          return new SolrDeletePcbProductDomainEventModel(solrDeleteProduct.getId());
        }).collect(Collectors.toList())
    );
    this.domainEventPublisherService.publishSolrBatchDeletePcbProductEvent(
        solrDeleteBatchPcbProductDomainEventModel);
  }

  private void setProductItemsDataInSolrEvent(Product product, List<String> upcCodes, List<String> skuCodes,
    List<String> generatedItemNames, List<Integer> dangerousGoodsLevels, List<String> locationPaths) {
    for (ProductItem productItem: product.getProductItems()) {
      if(!productItem.isMarkForDelete()) {
        upcCodes.add(productItem.getUpcCode());
        skuCodes.add(productItem.getSkuCode());
        generatedItemNames.add(productItem.getGeneratedItemName());
        dangerousGoodsLevels.add(productItem.getDangerousGoodsLevel());
        locationPaths.addAll(productItem.getProductItemImages().stream()
            .filter(productItemImage -> !productItemImage.isMarkForDelete())
            .filter(ProductItemImage::isMainImages)
            .map(ProductItemImage::getLocationPath).collect(Collectors.toList()));
      }
    }
  }

  private void partialReindexBrandWips(String storeId, Date startDate, Date currentDate, Pageable pageable)
      throws Exception {
    Page<BrandWip> brandWipPage;
    do {
      brandWipPage =
          this.brandWipRepository.findByStoreIdAndUpdatedDateBetween(storeId, startDate, currentDate, pageable);
      pageable = brandWipPage.nextPageable();
      processUpdatedBrandWips(brandWipPage.getContent());
    } while (brandWipPage.hasNext());
  }

  private void partialReindexActiveBrands(String storeId, Date startDate, Date currentDate, Pageable pageable)
      throws Exception {
    Page<Brand> activeBrands;
    Page<String> deletedBrands;
    do {
      activeBrands = this.brandRepository
          .findByStoreIdAndUpdatedDateBetweenAndMarkForDeleteFalse(storeId, startDate, currentDate, pageable);
      pageable = activeBrands.nextPageable();
      publishEventsToAddActiveBrandToSolr(activeBrands.getContent());
    } while (activeBrands.hasNext());
    pageable = PAGEABLE;
    do {
      deletedBrands = this.brandRepository
          .findIdByStoreIdAndUpdatedDateBetweenAndMarkForDeleteTrue(storeId, startDate, currentDate, pageable);
      pageable = deletedBrands.nextPageable();
      publishEventsToDeleteBrandWipsFromSolr(deletedBrands.getContent());
    } while (deletedBrands.hasNext());
  }

  private void processUpdatedBrandWips(List<BrandWip> brandWips) throws Exception {
    List<BrandWip> addedBrandWips = new ArrayList<>();
    List<String> rejectedBrandWips = new ArrayList<>();
    List<BrandWip> approvedBrandWips = new ArrayList<>();
    for (BrandWip brandWip : brandWips) {
      if (BrandWipState.DRAFT.equals(brandWip.getState())) {
        addedBrandWips.add(brandWip);
      } else if (BrandWipState.REJECTED.equals(brandWip.getState()) || BrandWipState.DELETED.equals(brandWip.getState())) {
        rejectedBrandWips.add(brandWip.getId());
      } else if (BrandWipState.APPROVED.equals(brandWip.getState())) {
        approvedBrandWips.add(brandWip);
      }
    }
    if (CollectionUtils.isNotEmpty(addedBrandWips)) {
      publishEventsToAddBrandWipToSolr(addedBrandWips);
    }
    if (CollectionUtils.isNotEmpty(rejectedBrandWips)) {
      publishEventsToDeleteBrandWipsFromSolr(rejectedBrandWips);
    }
    if (CollectionUtils.isNotEmpty(approvedBrandWips)) {
      publishEventsToAddApprovedBrandToSolr(approvedBrandWips);
    }
  }

  private void publishEventsToAddActiveBrandToSolr(List<Brand> brands) throws Exception {
    List<SolrBrandModel> solrBrandModels =
        brands.stream().map(ConverterUtil::generateSolrBrandModelForActiveBrand).collect(Collectors.toList());
    this.domainEventPublisherService
        .publishSolrAddBrandEvent(SolrAddBrandListDomainEventModel.builder().solrBrandModels(solrBrandModels).build());
  }

  private void publishEventsToAddApprovedBrandToSolr(List<BrandWip> brands) throws Exception {
    List<SolrBrandModel> solrBrandModels =
        brands.stream().map(ConverterUtil::generateSolrBrandModelApproveExistingBrand).collect(Collectors.toList());
    this.domainEventPublisherService
        .publishSolrAddBrandEvent(SolrAddBrandListDomainEventModel.builder().solrBrandModels(solrBrandModels).build());
  }

  private void publishEventsToAddBrandWipToSolr(List<BrandWip> brandWips) throws Exception {
    List<SolrBrandModel> solrBrandModels =
        brandWips.stream().map(ConverterUtil::generateSolrBrandModelModelForBrandWip).collect(Collectors.toList());
    this.domainEventPublisherService
        .publishSolrAddBrandEvent(SolrAddBrandListDomainEventModel.builder().solrBrandModels(solrBrandModels).build());
  }

  private void publishEventsToDeleteBrandWipsFromSolr(List<String> ids) throws Exception {
    SolrDeleteBrandDomainEventModel solrDeleteBrandDomainEventModel =
        ConverterUtil.generateSolrDeleteBrandDomainEventModel(ids);
    this.domainEventPublisherService.publishSolrDeleteBrandEvent(solrDeleteBrandDomainEventModel);
  }
}
