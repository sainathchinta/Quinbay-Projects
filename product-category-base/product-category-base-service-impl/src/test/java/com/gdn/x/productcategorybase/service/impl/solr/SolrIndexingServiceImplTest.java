package com.gdn.x.productcategorybase.service.impl.solr;

import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.util.Assert;

import com.gdn.x.productcategorybase.ApplicationConfigPropertyNames;
import com.gdn.x.productcategorybase.SolrConstants;
import com.gdn.x.productcategorybase.domain.event.model.SolrAddBatchPcbProductDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.SolrAddBrandListDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.SolrAddPcbProductDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.SolrDeleteBatchPcbProductDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.SolrDeleteBrandDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.SolrUpdateBrandDomainEventModel;
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

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

public class SolrIndexingServiceImplTest {

  @Mock
  private BrandRepository brandRepository;

  @Mock
  private BrandWipRepository brandWipRepository;

  @InjectMocks
  private SolrIndexingServiceImpl solrIndexingService;

  @Mock
  private DomainEventPublisherService domainEventPublisherService;

  @Mock
  private CategoryService categoryService;

  @Mock
  private CategoryServiceWrapper categoryServiceWrapper;

  @Mock
  private ProductService productService;

  @Mock
  private SolrBrandRepository solrBrandRepository;

  @Mock
  private SolrPcbRepository solrPcbRepository;

  @Captor
  private ArgumentCaptor<SolrAddBrandListDomainEventModel> solrAddBrandDomainEventModelArgumentCaptor;

  @Captor
  private ArgumentCaptor<SolrDeleteBrandDomainEventModel> solrDeleteBrandDomainEventModelArgumentCaptor;

  @Captor
  private ArgumentCaptor<SolrUpdateBrandDomainEventModel> solrUpdateBrandDomainEventModelArgumentCaptor;

  @Mock
  private ApplicationConfigPropertiesService applicationConfigPropertiesService;

  @Captor
  private ArgumentCaptor<SolrAddBatchPcbProductDomainEventModel> solrAddBatchPcbProductDomainEventModelArgumentCaptor;

  @Captor
  private ArgumentCaptor<ApplicationConfigProperties> applicationConfigPropertiesArgumentCaptor;

  @Captor
  private ArgumentCaptor<SolrDeleteBatchPcbProductDomainEventModel> solrDeleteBatchPcbProductDomainEventModelCaptor;

  private Page<Brand> brandPage;
  private Page<BrandWip> brandWipPage;
  private List<Brand> brandList;
  private List<BrandWip> brandWipList;
  private Pageable pageable;
  private SolrAddBrandListDomainEventModel solrAddBrandListDomainEventModel;
  private SolrBrandModel solrBrandModel;
  private SolrUpdateBrandDomainEventModel solrUpdateBrandDomainEventModel;
  private SolrDeleteBrandDomainEventModel solrDeleteBrandDomainEventModel;
  private List<String> brandIds;
  private Page<String> brandIdPage;
  private ApplicationConfigProperties applicationConfigProperties;
  private ApplicationConfigProperties applicationConfigPropertiesForPcbCollection;
  private List<Category> parentCategories;
  private Page<Product> productsPage;
  private List<Product> productsList;
  private Product product1;
  private Product product2;

  private static final String BRAND_CODE = "BR-00001";
  private static final int PAGE = 0;
  private static final int SIZE = 10;
  private static final String STORE_ID = "10001";
  private static final String ID = "id";
  private static final String PRODUCT_ID_1 = "productId1";
  private static final String PRODUCT_ID_2 = "productId2";
  private static final String PRODUCT_CODE_1 = "productCode1";
  private static final String PRODUCT_CODE_2 = "productCode2";
  private static final String PRODUCT_NAME_1 = "productName1";
  private static final String CATEGORY_ID_1 = "categoryId1";
  private static final String CATEGORY_ID_2 = "categoryId2";
  private static final String CATEGORY_ID_3 = "categoryId3";
  private static final String CATEGORY_CODE_1 = "categoryCode1";
  private static final String CATEGORY_CODE_2 = "categoryCode2";
  private static final String UPC_CODE_1 = "upcCode1";
  private static final String SKU_CODE_1 = "skuCode1";
  private static final String GENERATED_ITEM_NAME_1 = "generatedItemName1";
  private static final Integer DANGEROUS_GOODS_LEVEL_1 = 1;
  private static final String IMAGE_LOCATION_PATH_1 = "imageLocationPath";

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    pageable = PageRequest.of(PAGE, SIZE);
    Brand brand = new Brand();
    brand.setBrandCode(BRAND_CODE);
    brandList = Arrays.asList(brand);
    brandPage = new PageImpl<>(brandList);
    BrandWip brandWip = new BrandWip();
    brandWip.setState(BrandWipState.DRAFT);
    brandWip.setBrandRequestCode(BRAND_CODE);
    BrandWip brandWip1 = new BrandWip();
    brandWip1.setState(BrandWipState.APPROVED);
    brandWip.setBrandRequestCode(BRAND_CODE);
    BrandWip brandWip2 = new BrandWip();
    brandWip2.setState(BrandWipState.REJECTED);
    brandWip.setBrandRequestCode(BRAND_CODE);
    BrandWip brandWip3 = new BrandWip();
    brandWip3.setState(BrandWipState.DELETED);
    brandWipList = Arrays.asList(brandWip, brandWip1, brandWip2, brandWip3);
    brandWipPage = new PageImpl<>(brandWipList);
    Category category1 = new Category();
    category1.setId(CATEGORY_ID_1);
    category1.setCategoryCode(CATEGORY_CODE_1);
    Category category2 = new Category();
    category2.setId(CATEGORY_ID_2);
    category2.setCategoryCode(CATEGORY_CODE_2);
    parentCategories = Arrays.asList(category1, category2);
    product1 = new Product();
    product1.setStoreId(STORE_ID);
    product1.setId(PRODUCT_ID_1);
    product1.setName(PRODUCT_NAME_1);
    product1.setMarkForDelete(false);
    product1.setViewable(true);
    product1.setActivated(true);
    product1.setProductCode(PRODUCT_CODE_1);
    ProductCategory productCategory1 = new ProductCategory();
    productCategory1.setMarkForDelete(false);
    Category category3 = new Category();
    category3.setId(CATEGORY_ID_3);
    productCategory1.setCategory(category3);
    product1.setProductCategories(Collections.singletonList(productCategory1));
    ProductItem productItem1 = new ProductItem();
    productItem1.setUpcCode(UPC_CODE_1);
    productItem1.setSkuCode(SKU_CODE_1);
    productItem1.setDangerousGoodsLevel(DANGEROUS_GOODS_LEVEL_1);
    productItem1.setGeneratedItemName(GENERATED_ITEM_NAME_1);
    ProductItemImage productItemImage1 = new ProductItemImage();
    productItemImage1.setMainImages(true);
    productItemImage1.setMarkForDelete(false);
    productItemImage1.setLocationPath(IMAGE_LOCATION_PATH_1);
    ProductItemImage productItemImage2 = new ProductItemImage();
    productItemImage2.setMarkForDelete(false);
    productItemImage2.setMainImages(false);
    productItem1.setProductItemImages(Arrays.asList(productItemImage1, productItemImage2));
    product1.setProductItems(Collections.singletonList(productItem1));
    product1.setViewable(true);
    product1.setActivated(true);
    product1.setMarkForDelete(false);
    product2 = new Product();
    product2.setStoreId(STORE_ID);
    product2.setId(PRODUCT_ID_2);
    product2.setProductCode(PRODUCT_CODE_2);
    solrAddBrandListDomainEventModel = new SolrAddBrandListDomainEventModel();
    solrBrandModel = new SolrBrandModel();
    solrBrandModel.setBrandCode(BRAND_CODE);
    solrAddBrandListDomainEventModel.setSolrBrandModels(Arrays.asList(solrBrandModel));
    solrDeleteBrandDomainEventModel = new SolrDeleteBrandDomainEventModel();
    solrDeleteBrandDomainEventModel.setIds(Arrays.asList(ID));
    brandIds = Arrays.asList(ID);
    brandIdPage = new PageImpl<>(brandIds);
    applicationConfigProperties = new ApplicationConfigProperties();
    applicationConfigProperties.setValue(String.valueOf(new Date().getTime()));
    applicationConfigProperties.setPropertyName(ApplicationConfigPropertyNames.SOLR_BRAND_COLLECTION_LAST_DELTA_RUN_TIME);
    applicationConfigPropertiesForPcbCollection = new ApplicationConfigProperties();
    applicationConfigPropertiesForPcbCollection.setValue(String.valueOf(new Date().getTime() - 86400000));
    applicationConfigPropertiesForPcbCollection
        .setPropertyName(ApplicationConfigPropertyNames.SOLR_PCB_COLLECTION_LAST_DELTA_RUN_TIME);
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(brandRepository);
    verifyNoMoreInteractions(brandWipRepository);
    verifyNoMoreInteractions(domainEventPublisherService);
    verifyNoMoreInteractions(applicationConfigPropertiesService);
    verifyNoMoreInteractions(categoryService);
    verifyNoMoreInteractions(categoryServiceWrapper);
    verifyNoMoreInteractions(productService, solrBrandRepository, solrPcbRepository);
  }

  @Test
  public void fullReindexBrandCollectionTest() throws Exception {
    doNothing().when(this.solrBrandRepository).deleteAllDocumentsFromSolr();
    when(this.brandRepository.findByStoreIdAndMarkForDeleteFalse(eq(STORE_ID), any(PageRequest.class)))
        .thenReturn(brandPage);
    when(this.brandWipRepository
        .findByStoreIdAndStateAndMarkForDeleteFalse(eq(STORE_ID), eq(BrandWipState.DRAFT),
            any(PageRequest.class))).thenReturn(brandWipPage);
    when(this.domainEventPublisherService.publishSolrAddBrandEvent(any(SolrAddBrandListDomainEventModel.class)))
        .thenReturn(solrAddBrandListDomainEventModel);
    when(this.applicationConfigPropertiesService.findByStoreIdAndPropertyNameAndMarkForDeleteFalse(STORE_ID,
        ApplicationConfigPropertyNames.SOLR_BRAND_COLLECTION_LAST_DELTA_RUN_TIME))
        .thenReturn(applicationConfigProperties);
    when(this.applicationConfigPropertiesService.save(any(ApplicationConfigProperties.class)))
        .thenReturn(applicationConfigProperties);;
    this.solrIndexingService.fullReindexBrandCollection(STORE_ID);
    verify(this.brandRepository).findByStoreIdAndMarkForDeleteFalse(eq(STORE_ID), any(PageRequest.class));
    verify(this.brandWipRepository)
        .findByStoreIdAndStateAndMarkForDeleteFalse(eq(STORE_ID), eq(BrandWipState.DRAFT),
            any(PageRequest.class));
    verify(this.domainEventPublisherService, times(2))
        .publishSolrAddBrandEvent(solrAddBrandDomainEventModelArgumentCaptor.capture());
    verify(this.solrBrandRepository).deleteAllDocumentsFromSolr();
    verify(this.applicationConfigPropertiesService).findByStoreIdAndPropertyNameAndMarkForDeleteFalse(STORE_ID,
        ApplicationConfigPropertyNames.SOLR_BRAND_COLLECTION_LAST_DELTA_RUN_TIME);
    verify(this.applicationConfigPropertiesService).save(any(ApplicationConfigProperties.class));
  }

  @Test
  public void fullReindexBrandCollectionException() throws Exception {
    try {
      doNothing().when(this.solrBrandRepository).deleteAllDocumentsFromSolr();
      when(this.brandRepository.findByStoreIdAndMarkForDeleteFalse(eq(STORE_ID), any(PageRequest.class))).thenReturn(brandPage);
      when(this.brandWipRepository
          .findByStoreIdAndStateAndMarkForDeleteFalse(eq(STORE_ID), eq(BrandWipState.DRAFT), any
              (PageRequest.class))).thenReturn(brandWipPage);
      when(this.domainEventPublisherService.publishSolrAddBrandEvent(any(SolrAddBrandListDomainEventModel.class)))
          .thenThrow(Exception.class);
      this.solrIndexingService.fullReindexBrandCollection(STORE_ID);
    } catch (Exception e) {
    } finally {
      verify(this.brandRepository).findByStoreIdAndMarkForDeleteFalse(eq(STORE_ID), any(PageRequest.class));
      verify(this.domainEventPublisherService, times(1))
          .publishSolrAddBrandEvent(solrAddBrandDomainEventModelArgumentCaptor.capture());
      verify(this.solrBrandRepository).deleteAllDocumentsFromSolr();
    }
  }

  @Test
  public void partialReindexBrandCollectionTest() throws Exception {
    when(this.brandRepository
        .findByStoreIdAndUpdatedDateBetweenAndMarkForDeleteFalse(eq(STORE_ID), any(Date.class),
            any(Date.class), any(PageRequest.class))).thenReturn(brandPage);
    when(this.brandRepository
        .findIdByStoreIdAndUpdatedDateBetweenAndMarkForDeleteTrue(eq(STORE_ID), any(Date.class),
            any(Date.class), any(PageRequest.class))).thenReturn(brandIdPage);
    when(this.brandWipRepository
        .findByStoreIdAndUpdatedDateBetween(eq(STORE_ID), any(Date.class), any(Date.class),
            any(PageRequest.class))).thenReturn(brandWipPage);
    when(this.domainEventPublisherService.publishSolrAddBrandEvent(any(SolrAddBrandListDomainEventModel.class)))
        .thenReturn(solrAddBrandListDomainEventModel);
    when(this.domainEventPublisherService
        .publishSolrUpdateBrandEvent(any(SolrUpdateBrandDomainEventModel.class)))
        .thenReturn(solrUpdateBrandDomainEventModel);
    when(this.domainEventPublisherService
        .publishSolrDeleteBrandEvent(any(SolrDeleteBrandDomainEventModel.class)))
        .thenReturn(solrDeleteBrandDomainEventModel);
    when(this.applicationConfigPropertiesService.findByStoreIdAndPropertyNameAndMarkForDeleteFalse(STORE_ID,
        ApplicationConfigPropertyNames.SOLR_BRAND_COLLECTION_LAST_DELTA_RUN_TIME))
        .thenReturn(applicationConfigProperties);
    when(this.applicationConfigPropertiesService.save(any(ApplicationConfigProperties.class)))
        .thenReturn(applicationConfigProperties);
    this.solrIndexingService.partialReindexBrandCollection(STORE_ID);
    verify(this.brandRepository)
        .findByStoreIdAndUpdatedDateBetweenAndMarkForDeleteFalse(eq(STORE_ID), any(Date.class),
            any(Date.class), any(PageRequest.class));
    verify(this.brandRepository)
        .findIdByStoreIdAndUpdatedDateBetweenAndMarkForDeleteTrue(eq(STORE_ID), any(Date.class),
            any(Date.class), any(PageRequest.class));
    verify(this.brandWipRepository)
        .findByStoreIdAndUpdatedDateBetween(eq(STORE_ID), any(Date.class), any(Date.class),
            any(PageRequest.class));
    verify(this.domainEventPublisherService, times(3))
        .publishSolrAddBrandEvent(solrAddBrandDomainEventModelArgumentCaptor.capture());
    verify(this.domainEventPublisherService, times(2))
        .publishSolrDeleteBrandEvent(solrDeleteBrandDomainEventModelArgumentCaptor.capture());
    verify(this.applicationConfigPropertiesService).findByStoreIdAndPropertyNameAndMarkForDeleteFalse(STORE_ID,
        ApplicationConfigPropertyNames.SOLR_BRAND_COLLECTION_LAST_DELTA_RUN_TIME);
    verify(this.applicationConfigPropertiesService).save(any(ApplicationConfigProperties.class));
    Assertions.assertEquals(solrAddBrandDomainEventModelArgumentCaptor.getValue().getSolrBrandModels().get(0).getBusinessPartnerCode(),
        SolrConstants.NA);
    Assertions.assertEquals(solrAddBrandDomainEventModelArgumentCaptor.getValue().getSolrBrandModels().size(), 1);
  }

  @Test
  public void partialReindexBrandCollectionWithRejectedBrandsTest() throws Exception {
    when(this.brandRepository
        .findByStoreIdAndUpdatedDateBetweenAndMarkForDeleteFalse(eq(STORE_ID), any(Date.class),
            any(Date.class), any(PageRequest.class))).thenReturn(brandPage);
    when(this.brandRepository
        .findIdByStoreIdAndUpdatedDateBetweenAndMarkForDeleteTrue(eq(STORE_ID), any(Date.class),
            any(Date.class), any(PageRequest.class))).thenReturn(brandIdPage);
    when(this.brandWipRepository
        .findByStoreIdAndUpdatedDateBetween(eq(STORE_ID), any(Date.class), any(Date.class),
            any(PageRequest.class))).thenReturn(brandWipPage);
    when(this.domainEventPublisherService.publishSolrAddBrandEvent(any(SolrAddBrandListDomainEventModel.class)))
        .thenReturn(solrAddBrandListDomainEventModel);
    when(this.domainEventPublisherService
        .publishSolrUpdateBrandEvent(any(SolrUpdateBrandDomainEventModel.class)))
        .thenReturn(solrUpdateBrandDomainEventModel);
    when(this.domainEventPublisherService
        .publishSolrDeleteBrandEvent(any(SolrDeleteBrandDomainEventModel.class)))
        .thenReturn(solrDeleteBrandDomainEventModel);
    when(this.applicationConfigPropertiesService.findByStoreIdAndPropertyNameAndMarkForDeleteFalse(STORE_ID,
        ApplicationConfigPropertyNames.SOLR_BRAND_COLLECTION_LAST_DELTA_RUN_TIME))
        .thenReturn(applicationConfigProperties);
    when(this.applicationConfigPropertiesService.save(any(ApplicationConfigProperties.class)))
        .thenReturn(applicationConfigProperties);
    this.solrIndexingService.partialReindexBrandCollection(STORE_ID);
    verify(this.brandRepository)
        .findByStoreIdAndUpdatedDateBetweenAndMarkForDeleteFalse(eq(STORE_ID), any(Date.class),
            any(Date.class), any(PageRequest.class));
    verify(this.brandRepository)
        .findIdByStoreIdAndUpdatedDateBetweenAndMarkForDeleteTrue(eq(STORE_ID), any(Date.class),
            any(Date.class), any(PageRequest.class));
    verify(this.brandWipRepository)
        .findByStoreIdAndUpdatedDateBetween(eq(STORE_ID), any(Date.class), any(Date.class),
            any(PageRequest.class));
    verify(this.domainEventPublisherService, times(3))
        .publishSolrAddBrandEvent(solrAddBrandDomainEventModelArgumentCaptor.capture());
    verify(this.domainEventPublisherService, times(2))
        .publishSolrDeleteBrandEvent(solrDeleteBrandDomainEventModelArgumentCaptor.capture());
    verify(this.applicationConfigPropertiesService).findByStoreIdAndPropertyNameAndMarkForDeleteFalse(STORE_ID,
        ApplicationConfigPropertyNames.SOLR_BRAND_COLLECTION_LAST_DELTA_RUN_TIME);
    verify(this.applicationConfigPropertiesService).save(any(ApplicationConfigProperties.class));
  }

  @Test
  public void fullReindexPcbCollectionTest() throws Exception {
    productsList = Collections.singletonList(product1);
    productsPage = new PageImpl<>(productsList);
    doNothing().when(this.solrPcbRepository).deleteAllDocumentsFromSolr();
    when(categoryService.findAllParentCategories(STORE_ID)).thenReturn(parentCategories);
    when(productService.findByViewableAndActivatedWithItemsInitialized(eq(STORE_ID), eq(true), eq(true),
        any(Pageable.class))).thenReturn(productsPage);
    when(categoryServiceWrapper.getFinalParentCategoryCached(STORE_ID, CATEGORY_ID_3)).thenReturn(CATEGORY_CODE_1);
    when(applicationConfigPropertiesService.findByStoreIdAndPropertyNameAndMarkForDeleteFalse(STORE_ID,
        ApplicationConfigPropertyNames.SOLR_PCB_COLLECTION_LAST_DELTA_RUN_TIME))
        .thenReturn(applicationConfigPropertiesForPcbCollection);
    solrIndexingService.setPcbCollectionReindexBatchSize(5);
    solrIndexingService.fullReindexPCBCollection(STORE_ID);
    verify(categoryService).findAllParentCategories(STORE_ID);
    verify(productService).findByViewableAndActivatedWithItemsInitialized(eq(STORE_ID), eq(true), eq(true),
        any(Pageable.class));
    verify(categoryServiceWrapper).getFinalParentCategoryCached(STORE_ID, CATEGORY_ID_3);
    verify(domainEventPublisherService)
        .publishSolrAddBatchPcbProductEvent(solrAddBatchPcbProductDomainEventModelArgumentCaptor.capture());
    verify(this.solrPcbRepository).deleteAllDocumentsFromSolr();
    verify(applicationConfigPropertiesService).findByStoreIdAndPropertyNameAndMarkForDeleteFalse(STORE_ID,
        ApplicationConfigPropertyNames.SOLR_PCB_COLLECTION_LAST_DELTA_RUN_TIME);
    verify(applicationConfigPropertiesService).save(applicationConfigPropertiesArgumentCaptor.capture());
    SolrAddPcbProductDomainEventModel product = solrAddBatchPcbProductDomainEventModelArgumentCaptor
        .getValue().getProductDomainEventModelList().get(0);
    Assertions.assertEquals(PRODUCT_ID_1, product.getId());
    Assertions.assertEquals(PRODUCT_CODE_1, product.getProductCode());
    Assertions.assertEquals(PRODUCT_NAME_1, product.getName());
    Assertions.assertEquals(CATEGORY_ID_3, product.getCategoryId());
    Assertions.assertEquals(CATEGORY_ID_1, product.getParentCategoryId());
    Assertions.assertEquals(Arrays.asList(UPC_CODE_1), product.getUpcCodes());
    Assertions.assertEquals(Arrays.asList(SKU_CODE_1), product.getSkuCodes());
    Assertions.assertEquals(Arrays.asList(DANGEROUS_GOODS_LEVEL_1), product.getDangerousGoodsLevels());
    Assertions.assertEquals(Arrays.asList(GENERATED_ITEM_NAME_1), product.getGeneratedItemNames());
    Assertions.assertEquals(Arrays.asList(IMAGE_LOCATION_PATH_1), product.getImageLocationPaths());
  }

  @Test
  public void deltaReindexPcbCollectionTest() throws Exception {
    productsList = Arrays.asList(product1, product2);
    productsPage = new PageImpl<>(productsList);
    when(categoryService.findAllParentCategories(STORE_ID)).thenReturn(parentCategories);
    when(productService.findProductsByStoreIdAndUpdatedDateBetweenWithInitialization(
        eq(STORE_ID), any(Date.class), any(Date.class), any(Pageable.class)))
        .thenReturn(productsPage);
    when(categoryServiceWrapper.getFinalParentCategoryCached(STORE_ID, CATEGORY_ID_3)).thenReturn(CATEGORY_CODE_1);
    when(applicationConfigPropertiesService.findByStoreIdAndPropertyNameAndMarkForDeleteFalse(STORE_ID,
            ApplicationConfigPropertyNames.SOLR_PCB_COLLECTION_LAST_DELTA_RUN_TIME))
        .thenReturn(applicationConfigPropertiesForPcbCollection);
    Date startDate = new Date(Long.parseLong(applicationConfigPropertiesForPcbCollection.getValue()));
    solrIndexingService.deltaReindexPCBCollection(STORE_ID);
    verify(categoryService).findAllParentCategories(STORE_ID);
    verify(productService).findProductsByStoreIdAndUpdatedDateBetweenWithInitialization(
        eq(STORE_ID), any(Date.class), any(Date.class), any(Pageable.class));
    verify(categoryServiceWrapper).getFinalParentCategoryCached(STORE_ID, CATEGORY_ID_3);
    verify(applicationConfigPropertiesService).findByStoreIdAndPropertyNameAndMarkForDeleteFalse(STORE_ID,
        ApplicationConfigPropertyNames.SOLR_PCB_COLLECTION_LAST_DELTA_RUN_TIME);
    verify(domainEventPublisherService)
        .publishSolrAddBatchPcbProductEvent(solrAddBatchPcbProductDomainEventModelArgumentCaptor.capture());
    verify(domainEventPublisherService)
        .publishSolrBatchDeletePcbProductEvent(solrDeleteBatchPcbProductDomainEventModelCaptor.capture());
    verify(applicationConfigPropertiesService).save(applicationConfigPropertiesArgumentCaptor.capture());
    SolrAddPcbProductDomainEventModel product = solrAddBatchPcbProductDomainEventModelArgumentCaptor
        .getValue().getProductDomainEventModelList().get(0);
    Assertions.assertEquals(PRODUCT_ID_1, product.getId());
    Assertions.assertEquals(PRODUCT_CODE_1, product.getProductCode());
    Assertions.assertEquals(PRODUCT_NAME_1, product.getName());
    Assertions.assertEquals(CATEGORY_ID_3, product.getCategoryId());
    Assertions.assertEquals(CATEGORY_ID_1, product.getParentCategoryId());
    Assertions.assertEquals(Arrays.asList(UPC_CODE_1), product.getUpcCodes());
    Assertions.assertEquals(Arrays.asList(SKU_CODE_1), product.getSkuCodes());
    Assertions.assertEquals(Arrays.asList(DANGEROUS_GOODS_LEVEL_1), product.getDangerousGoodsLevels());
    Assertions.assertEquals(Arrays.asList(GENERATED_ITEM_NAME_1), product.getGeneratedItemNames());
    Assertions.assertEquals(Arrays.asList(IMAGE_LOCATION_PATH_1), product.getImageLocationPaths());
    Assertions.assertEquals(PRODUCT_ID_2, solrDeleteBatchPcbProductDomainEventModelCaptor.getValue()
        .getSolrDeletePcbProductDomainEventModels().get(0).getId());
    Assertions.assertNotEquals(String.valueOf(startDate.getTime()),
        applicationConfigPropertiesArgumentCaptor.getValue().getValue());
  }

  @Test
  public void categoryBasedReindexPCBCollection() throws Exception {
    productsList = Arrays.asList(product1, product2);
    productsPage = new PageImpl<>(productsList);

    when(categoryService.getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(STORE_ID, CATEGORY_CODE_1))
            .thenReturn(parentCategories.get(0));
    when(productService.findByCategoryIdWithItemsInitialized(eq(STORE_ID), eq(CATEGORY_ID_1),
            any(Pageable.class))).thenReturn(productsPage);
    when(categoryServiceWrapper.getFinalParentCategoryCached(STORE_ID, CATEGORY_ID_3)).thenReturn(CATEGORY_CODE_1);

    solrIndexingService.categoryBasedReindexPCBCollection(STORE_ID, CATEGORY_CODE_1);

    verify(categoryService).getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(STORE_ID, CATEGORY_CODE_1);
    verify(productService).findByCategoryIdWithItemsInitialized(eq(STORE_ID), eq(CATEGORY_ID_1), any(Pageable.class));
    verify(categoryServiceWrapper).getFinalParentCategoryCached(STORE_ID, CATEGORY_ID_3);
    verify(domainEventPublisherService)
            .publishSolrAddBatchPcbProductEvent(solrAddBatchPcbProductDomainEventModelArgumentCaptor.capture());

    SolrAddPcbProductDomainEventModel product = solrAddBatchPcbProductDomainEventModelArgumentCaptor
            .getValue().getProductDomainEventModelList().get(0);
    Assertions.assertEquals(PRODUCT_ID_1, product.getId());
    Assertions.assertEquals(PRODUCT_CODE_1, product.getProductCode());
    Assertions.assertEquals(PRODUCT_NAME_1, product.getName());
    Assertions.assertEquals(CATEGORY_ID_3, product.getCategoryId());
    Assertions.assertEquals(CATEGORY_ID_1, product.getParentCategoryId());
    Assertions.assertEquals(Arrays.asList(UPC_CODE_1), product.getUpcCodes());
    Assertions.assertEquals(Arrays.asList(SKU_CODE_1), product.getSkuCodes());
    Assertions.assertEquals(Arrays.asList(DANGEROUS_GOODS_LEVEL_1), product.getDangerousGoodsLevels());
    Assertions.assertEquals(Arrays.asList(GENERATED_ITEM_NAME_1), product.getGeneratedItemNames());
    Assertions.assertEquals(Arrays.asList(IMAGE_LOCATION_PATH_1), product.getImageLocationPaths());
  }

  @Test
  public void categoryBasedReindexWithReviewPendingFlagPCBCollection() throws Exception {
    product2.setReviewPending(true);
    product2.setActivated(true);
    product2.setViewable(true);
    productsList = Arrays.asList(product1, product2);
    productsPage = new PageImpl<>(productsList);

    when(categoryService.getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(STORE_ID, CATEGORY_CODE_1))
        .thenReturn(parentCategories.get(0));
    when(productService.findByCategoryIdWithItemsInitialized(eq(STORE_ID), eq(CATEGORY_ID_1),
        any(Pageable.class))).thenReturn(productsPage);
    when(categoryServiceWrapper.getFinalParentCategoryCached(STORE_ID, CATEGORY_ID_3)).thenReturn(CATEGORY_CODE_1);

    solrIndexingService.categoryBasedReindexPCBCollection(STORE_ID, CATEGORY_CODE_1);

    verify(categoryService).getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(STORE_ID, CATEGORY_CODE_1);
    verify(productService).findByCategoryIdWithItemsInitialized(eq(STORE_ID), eq(CATEGORY_ID_1), any(Pageable.class));
    verify(categoryServiceWrapper).getFinalParentCategoryCached(STORE_ID, CATEGORY_ID_3);
    verify(domainEventPublisherService)
        .publishSolrAddBatchPcbProductEvent(solrAddBatchPcbProductDomainEventModelArgumentCaptor.capture());

    SolrAddPcbProductDomainEventModel product = solrAddBatchPcbProductDomainEventModelArgumentCaptor
        .getValue().getProductDomainEventModelList().get(0);
    Assertions.assertEquals(PRODUCT_ID_1, product.getId());
    Assertions.assertEquals(PRODUCT_CODE_1, product.getProductCode());
    Assertions.assertEquals(PRODUCT_NAME_1, product.getName());
    Assertions.assertEquals(CATEGORY_ID_3, product.getCategoryId());
    Assertions.assertEquals(CATEGORY_ID_1, product.getParentCategoryId());
    Assertions.assertEquals(Arrays.asList(UPC_CODE_1), product.getUpcCodes());
    Assertions.assertEquals(Arrays.asList(SKU_CODE_1), product.getSkuCodes());
    Assertions.assertEquals(Arrays.asList(DANGEROUS_GOODS_LEVEL_1), product.getDangerousGoodsLevels());
    Assertions.assertEquals(Arrays.asList(GENERATED_ITEM_NAME_1), product.getGeneratedItemNames());
    Assertions.assertEquals(Arrays.asList(IMAGE_LOCATION_PATH_1), product.getImageLocationPaths());
    Assertions.assertEquals(1,
        solrAddBatchPcbProductDomainEventModelArgumentCaptor.getValue().getProductDomainEventModelList().size());
  }

  @Test
  public void categoryBasedReindexPCBCollection_whenNoCategoryFound() throws Exception {
    when(categoryService.getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(STORE_ID, CATEGORY_CODE_1))
      .thenReturn(null);

    solrIndexingService.categoryBasedReindexPCBCollection(STORE_ID, CATEGORY_CODE_1);

    verify(categoryService).getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(STORE_ID, CATEGORY_CODE_1);
  }

  @Test
  public void categoryBasedReindexPCBCollection_whenNoProductsFoundForCategory() throws Exception {
    when(categoryService.getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(STORE_ID, CATEGORY_CODE_1))
      .thenReturn(parentCategories.get(0));

    when(productService.findByCategoryIdWithItemsInitialized(eq(STORE_ID), eq(CATEGORY_ID_1),
      any(Pageable.class))).thenReturn(null);

    solrIndexingService.categoryBasedReindexPCBCollection(STORE_ID, CATEGORY_CODE_1);

    verify(categoryService).getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(STORE_ID, CATEGORY_CODE_1);
    verify(productService).findByCategoryIdWithItemsInitialized(eq(STORE_ID), eq(CATEGORY_ID_1), any(Pageable.class));
  }

  @Test
  public void categoryBasedReindexPCBCollection_whenProductsLessThanBatchSize() throws Exception {
    productsList = Arrays.asList(product1, product2);
    productsPage = new PageImpl<>(productsList);

    when(categoryService.getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(STORE_ID, CATEGORY_CODE_1))
      .thenReturn(parentCategories.get(0));
    when(productService.findByCategoryIdWithItemsInitialized(eq(STORE_ID), eq(CATEGORY_ID_1),
      any(Pageable.class))).thenReturn(productsPage);
    when(categoryServiceWrapper.getFinalParentCategoryCached(STORE_ID, CATEGORY_ID_3)).thenReturn(CATEGORY_CODE_1);

    solrIndexingService.setPcbCollectionReindexBatchSize(3);

    solrIndexingService.categoryBasedReindexPCBCollection(STORE_ID, CATEGORY_CODE_1);

    verify(categoryService).getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(STORE_ID, CATEGORY_CODE_1);
    verify(productService).findByCategoryIdWithItemsInitialized(eq(STORE_ID), eq(CATEGORY_ID_1), any(Pageable.class));
    verify(categoryServiceWrapper).getFinalParentCategoryCached(STORE_ID, CATEGORY_ID_3);

    verify(domainEventPublisherService)
      .publishSolrAddBatchPcbProductEvent(solrAddBatchPcbProductDomainEventModelArgumentCaptor.capture());

    SolrAddPcbProductDomainEventModel product = solrAddBatchPcbProductDomainEventModelArgumentCaptor
      .getValue().getProductDomainEventModelList().get(0);

    Assertions.assertEquals(PRODUCT_ID_1, product.getId());
    Assertions.assertEquals(PRODUCT_CODE_1, product.getProductCode());
    Assertions.assertEquals(PRODUCT_NAME_1, product.getName());
    Assertions.assertEquals(CATEGORY_ID_3, product.getCategoryId());
    Assertions.assertEquals(CATEGORY_ID_1, product.getParentCategoryId());
    Assertions.assertEquals(Arrays.asList(UPC_CODE_1), product.getUpcCodes());
    Assertions.assertEquals(Arrays.asList(SKU_CODE_1), product.getSkuCodes());
    Assertions.assertEquals(Arrays.asList(DANGEROUS_GOODS_LEVEL_1), product.getDangerousGoodsLevels());
    Assertions.assertEquals(Arrays.asList(GENERATED_ITEM_NAME_1), product.getGeneratedItemNames());
    Assertions.assertEquals(Arrays.asList(IMAGE_LOCATION_PATH_1), product.getImageLocationPaths());
  }

  @Test
  public void reindexBrandCollectionByBrandRequestCodeTest() throws Exception {
    BrandWip brandWip = brandWipPage.getContent().get(0);
    brandWip.setState(BrandWipState.DELETED);
    Mockito.when(brandWipRepository.findByStoreIdAndBrandRequestCode(STORE_ID, BRAND_CODE))
        .thenReturn(brandWip);
    solrIndexingService.reindexBrandCollectionByBrandRequestCode(STORE_ID, BRAND_CODE);
    Mockito.verify(brandWipRepository).findByStoreIdAndBrandRequestCode(STORE_ID, BRAND_CODE);
    verify(this.domainEventPublisherService)
        .publishSolrDeleteBrandEvent(solrDeleteBrandDomainEventModelArgumentCaptor.capture());
  }

  @Test
  public void reindexBrandCollectionByBrandRequestCodeTest2() throws Exception {
    BrandWip brandWip = brandWipPage.getContent().get(0);
    brandWip.setState(BrandWipState.APPROVED);
    Mockito.when(brandWipRepository.findByStoreIdAndBrandRequestCode(STORE_ID, BRAND_CODE))
        .thenReturn(brandWip);
    solrIndexingService.reindexBrandCollectionByBrandRequestCode(STORE_ID, BRAND_CODE);
    Mockito.verify(brandWipRepository).findByStoreIdAndBrandRequestCode(STORE_ID, BRAND_CODE);
    verify(this.domainEventPublisherService)
        .publishSolrAddBrandEvent(solrAddBrandDomainEventModelArgumentCaptor.capture());
  }

  @Test
  public void deltaReindexPCBCollectionByProductCodeTest() throws Exception {
    Mockito.when(productService.getProductWithProductCategoriesAndItemsByStoreIdAndProductCodes(STORE_ID,
        Collections.singletonList(PRODUCT_CODE_1))).thenReturn(Collections.singletonList(product1));
    solrIndexingService.deltaReindexPCBCollectionByProductCode(STORE_ID, PRODUCT_CODE_1);
    Mockito.verify(productService).getProductWithProductCategoriesAndItemsByStoreIdAndProductCodes(STORE_ID,
        Collections.singletonList(PRODUCT_CODE_1));
    Mockito.verify(categoryService).findAllParentCategories(STORE_ID);
    verify(categoryServiceWrapper).getFinalParentCategoryCached(STORE_ID, CATEGORY_ID_3);
    verify(domainEventPublisherService)
        .publishSolrAddBatchPcbProductEvent(solrAddBatchPcbProductDomainEventModelArgumentCaptor.capture());
  }

  @Test
  public void deltaReindexPCBCollectionByProductCodeMFDTrueTest() throws Exception {
    product1.setMarkForDelete(true);
    Mockito.when(productService.getProductWithProductCategoriesAndItemsByStoreIdAndProductCodes(STORE_ID,
        Collections.singletonList(PRODUCT_CODE_1))).thenReturn(Collections.singletonList(product1));
    solrIndexingService.deltaReindexPCBCollectionByProductCode(STORE_ID, PRODUCT_CODE_1);
    Mockito.verify(productService).getProductWithProductCategoriesAndItemsByStoreIdAndProductCodes(STORE_ID,
        Collections.singletonList(PRODUCT_CODE_1));
    Mockito.verify(categoryService).findAllParentCategories(STORE_ID);
    verify(domainEventPublisherService)
        .publishSolrBatchDeletePcbProductEvent(solrDeleteBatchPcbProductDomainEventModelCaptor.capture());
  }

  @Test
  public void deltaReindexPCBCollectionByProductCodeViewableFalseTest() throws Exception {
    product1.setViewable(false);
    Mockito.when(productService.getProductWithProductCategoriesAndItemsByStoreIdAndProductCodes(STORE_ID,
        Collections.singletonList(PRODUCT_CODE_1))).thenReturn(Collections.singletonList(product1));
    solrIndexingService.deltaReindexPCBCollectionByProductCode(STORE_ID, PRODUCT_CODE_1);
    Mockito.verify(productService).getProductWithProductCategoriesAndItemsByStoreIdAndProductCodes(STORE_ID,
        Collections.singletonList(PRODUCT_CODE_1));
    Mockito.verify(categoryService).findAllParentCategories(STORE_ID);
    verify(domainEventPublisherService)
        .publishSolrBatchDeletePcbProductEvent(solrDeleteBatchPcbProductDomainEventModelCaptor.capture());
  }

  @Test
  public void deltaReindexPCBCollectionByProductCodeActivatedFalseTest() throws Exception {
    product1.setViewable(true);
    product1.setActivated(false);
    Mockito.when(productService.getProductWithProductCategoriesAndItemsByStoreIdAndProductCodes(STORE_ID,
        Collections.singletonList(PRODUCT_CODE_1))).thenReturn(Collections.singletonList(product1));
    solrIndexingService.deltaReindexPCBCollectionByProductCode(STORE_ID, PRODUCT_CODE_1);
    Mockito.verify(productService).getProductWithProductCategoriesAndItemsByStoreIdAndProductCodes(STORE_ID,
        Collections.singletonList(PRODUCT_CODE_1));
    Mockito.verify(categoryService).findAllParentCategories(STORE_ID);
    verify(domainEventPublisherService)
        .publishSolrBatchDeletePcbProductEvent(solrDeleteBatchPcbProductDomainEventModelCaptor.capture());
  }

}