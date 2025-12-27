package com.gdn.x.productcategorybase.service.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;

import org.apache.commons.collections.CollectionUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import com.gdn.common.util.BeanUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.util.GdnUUIDHelper;
import com.gdn.x.productcategorybase.AttributeType;
import com.gdn.x.productcategorybase.dto.AddProductAttributesDTO;
import com.gdn.x.productcategorybase.dto.NewAttributeRequestDTO;
import com.gdn.x.productcategorybase.entity.Attribute;
import com.gdn.x.productcategorybase.entity.Catalog;
import com.gdn.x.productcategorybase.entity.Category;
import com.gdn.x.productcategorybase.entity.PredefinedAllowedAttributeValue;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductAttribute;
import com.gdn.x.productcategorybase.entity.ProductAttributeValue;
import com.gdn.x.productcategorybase.entity.ProductCategory;
import com.gdn.x.productcategorybase.repository.AttributeRepository;
import com.gdn.x.productcategorybase.repository.CategoryRepository;
import com.gdn.x.productcategorybase.repository.PredefinedAllowedAttributeValueRepository;
import com.gdn.x.productcategorybase.repository.ProductAttributeRepository;
import com.gdn.x.productcategorybase.repository.ProductAttributeValueRepository;
import com.gdn.x.productcategorybase.repository.ProductCategoryRepository;
import com.gdn.x.productcategorybase.repository.ProductRepository;
import com.gdn.x.productcategorybase.service.CategoryService;
import com.gdn.x.productcategorybase.service.DomainEventPublisherService;
import com.gdn.x.productcategorybase.service.ProductService;

public class ProductCategoryServiceTest {
  private static final int PAGE_SIZE = 10;

  private static final int PAGE_NUMBER = 0;

  private static final Pageable DEFAULT_PAGE_REQUEST = PageRequest.of(0, 10);

  private static final String ID = "ID";
  private static final String CATEGORY_CODE = "CAT-1234";
  private static final String CATEGORY_ID = "1234";
  private static final String PRODUCT_CODE = "MTA-1234";
  private static final String STORE_ID = "STORE_ID";
  private static final String ATTR_CODE = "ATR-123";
  private static final String ATTR_NAME = "ATR NAME";

  private static final String ERROR_MESSAGE_FOR_SAVE = "use update for existing entity";
  private static final String ERROR_MESSAGE_FOR_UPDATE = "can not update un existence data";
  private static final String MASTER_CATALOG_CODE = "10001";

  @InjectMocks
  ProductCategoryServiceBean service;

  @Mock
  ProductCategoryRepository repository;
  @Mock
  private ProductRepository productRepository;
  @Mock
  private CategoryRepository categoryRepostory;
  @Mock
  private ProductCategoryRepository prdCategoryRepository;
  @Mock
  private ProductAttributeRepository prdAttributeRepository;
  @Mock
  private ProductAttributeValueRepository prdAttrValueRepository;
  @Mock
  private AttributeRepository attrRepository;
  @Mock
  private PredefinedAllowedAttributeValueRepository predefAllowedAttrRepository;
  @Mock
  private ApplicationCacheServiceBean applicationCacheServiceBean;
  @Mock
  private DomainEventPublisherService domainEventPublisherService;
  @Mock
  private ProductService productService;
  @Mock
  private CategoryService categoryService;
  
  private ProductCategory productCategory;
  private Pageable pageable;
  private Page<ProductCategory> productCategoryPage;
  private Product product;
  private Attribute attr;
  private PredefinedAllowedAttributeValue prdAttrValue;
  private List<Object[]> objects;
  private Category category;

  @BeforeEach
  public void setUp() {
    this.pageable =
        PageRequest.of(ProductCategoryServiceTest.PAGE_NUMBER, ProductCategoryServiceTest.PAGE_SIZE);
    MockitoAnnotations.initMocks(this);
    product = new Product();
    Catalog catalog = new Catalog();
    catalog.setCatalogCode(MASTER_CATALOG_CODE);
    category = new Category();
    category.setCategoryCode(CATEGORY_CODE);
    category.setCatalog(catalog);
    this.productCategory = new ProductCategory(product, category, ProductCategoryServiceTest.STORE_ID);
    this.productCategory.setId(ProductCategoryServiceTest.ID);
    List<ProductCategory> productCategories = new ArrayList<ProductCategory>();
    productCategories.add(this.productCategory);
    this.productCategoryPage = new PageImpl<ProductCategory>(productCategories,
        ProductCategoryServiceTest.DEFAULT_PAGE_REQUEST, productCategories.size());
    product.setId(ID);
    product.setProductCategories(new ArrayList<ProductCategory>());
    product.setProductCode(PRODUCT_CODE);
    product.setProductCategories(productCategories);
    
    attr = new Attribute();
    attr.setAttributeCode(ATTR_CODE);
    attr.setName(ATTR_NAME);
    
    prdAttrValue = new PredefinedAllowedAttributeValue();
    Object arr[] = new Object[2];
    objects = new ArrayList<>();
    objects.add(arr);;
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(this.repository, domainEventPublisherService, productService,
        categoryService);
  }

  @Test
  public void testDeleteProductCategory() throws Exception {
    this.service.delete(null);
  }

  @Test
  public void testFindById() throws Exception {
    Mockito.when(this.repository.findById(ProductCategoryServiceTest.ID)).thenReturn(Optional.of(this.productCategory));
    ProductCategory savedProductCategory = this.service.findById(ProductCategoryServiceTest.ID);
    Assertions.assertEquals(savedProductCategory, (this.productCategory));
    Mockito.verify(this.repository, Mockito.times(1)).findById(ProductCategoryServiceTest.ID);
  }

  @Test
  public void testFindByStoreId() throws Exception {
    Mockito.when(this.repository.findByStoreIdAndMarkForDeleteFalse(ProductCategoryServiceTest.STORE_ID, this.pageable))
        .thenReturn(this.productCategoryPage);
    Assertions.assertEquals(
        this.service.findByStoreId(ProductCategoryServiceTest.STORE_ID, this.pageable).getNumberOfElements(),
        (1));
    Mockito.verify(this.repository).findByStoreIdAndMarkForDeleteFalse(ProductCategoryServiceTest.STORE_ID,
        this.pageable);
  }

  @Test
  public void testFindByStoreIdAndId() throws Exception {
    Mockito.when(this.repository.findByStoreIdAndIdAndMarkForDeleteFalse(ProductCategoryServiceTest.STORE_ID,
        ProductCategoryServiceTest.ID)).thenReturn(this.productCategory);
    ProductCategory savedProductCategory =
        this.service.findByStoreIdAndId(ProductCategoryServiceTest.STORE_ID, ProductCategoryServiceTest.ID);
    Assertions.assertEquals(savedProductCategory, (this.productCategory));
    Mockito.verify(this.repository, Mockito.times(1))
        .findByStoreIdAndIdAndMarkForDeleteFalse(ProductCategoryServiceTest.STORE_ID, ProductCategoryServiceTest.ID);
  }

  @Test
  public void testSaveProductCategorySuccessfully() throws Exception {
    this.productCategory.setId(null);
    ProductCategory savedProductCategory = new ProductCategory();
    String uuid = GdnUUIDHelper.generateUUID();
    BeanUtils.copyProperties(this.productCategory, savedProductCategory);
    savedProductCategory.setId(uuid);
    Mockito.when(this.repository.saveAndFlush(this.productCategory)).thenReturn(savedProductCategory);
    Assertions.assertEquals(this.service.save(this.productCategory), (uuid));
    Mockito.verify(this.repository, Mockito.times(1)).saveAndFlush(this.productCategory);
  }

  @Test
  public void testSaveProductCategoryWithEmptyId() {
    ProductCategory savedProductCategory = new ProductCategory();
    String uuid = GdnUUIDHelper.generateUUID();
    this.productCategory.setId(uuid);
    BeanUtils.copyProperties(this.productCategory, savedProductCategory);
    Mockito.when(this.repository.findById(this.productCategory.getId())).thenReturn(Optional.of(savedProductCategory));
    try {
      this.service.save(this.productCategory);
    } catch (Exception e) {
      Assertions.assertTrue(e instanceof ApplicationRuntimeException);
      ApplicationRuntimeException applicationRuntimeException = (ApplicationRuntimeException) e;
      if (applicationRuntimeException.getErrorMessage().contains(ProductCategoryServiceTest.ERROR_MESSAGE_FOR_SAVE)) {
        Assertions.assertTrue(true);
      } else {
        Assertions.assertTrue(false);
      }
      Mockito.verify(this.repository, Mockito.times(1)).findById(this.productCategory.getId());
    }

  }

  @Test
  public void testUpdateProductCategoryNonExistenceEntity() {
    ProductCategory savedProductCategory = new ProductCategory();
    String uuid = GdnUUIDHelper.generateUUID();
    this.productCategory.setId(uuid);
    BeanUtils.copyProperties(this.productCategory, savedProductCategory);
    Mockito.when(this.repository.findById(uuid)).thenReturn(Optional.ofNullable(null));
    try {
      this.service.update(this.productCategory);
    } catch (Exception e) {
      Assertions.assertTrue(e instanceof ApplicationRuntimeException);
      ApplicationRuntimeException applicationRuntimeException = (ApplicationRuntimeException) e;
      if (applicationRuntimeException.getErrorMessage().contains(ProductCategoryServiceTest.ERROR_MESSAGE_FOR_UPDATE)) {
        Assertions.assertTrue(true);
      } else {
        Assertions.assertTrue(false);
      }
      Mockito.verify(this.repository, Mockito.times(1)).findById(this.productCategory.getId());
    }
  }

  @Test
  public void testUpdateProductCategorySuccessfully() throws Exception {
    ProductCategory savedProductCategory = new ProductCategory();
    String uuid = GdnUUIDHelper.generateUUID();
    this.productCategory.setId(uuid);
    BeanUtils.copyProperties(this.productCategory, savedProductCategory);
    Mockito.when(this.repository.findById(this.productCategory.getId())).thenReturn(Optional.of(savedProductCategory));
    this.service.update(this.productCategory);
    Assertions.assertTrue(true);
    Mockito.verify(this.repository, Mockito.times(1)).findById(this.productCategory.getId());
    Mockito.verify(this.repository, Mockito.times(1)).saveAndFlush(this.productCategory);

  }

  @Test
  public void testUpdateProductCategoryWithEmptyId() {
    this.productCategory.setId(null);
    ProductCategory savedProductCategory = new ProductCategory();
    BeanUtils.copyProperties(this.productCategory, savedProductCategory);
    try {
      this.service.update(this.productCategory);
    } catch (Exception e) {
      Assertions.assertTrue(e instanceof ApplicationRuntimeException);
      ApplicationRuntimeException applicationRuntimeException = (ApplicationRuntimeException) e;
      if (applicationRuntimeException.getErrorMessage().contains(ProductCategoryServiceTest.ERROR_MESSAGE_FOR_UPDATE)) {
        Assertions.assertTrue(true);
      } else {
        Assertions.assertTrue(false);
      }
    }
  }
  
  @Test
  public void movePrdCategoryByProductCode_Valid_Success() throws Exception {
    Product product = new Product();
    product.setId(ID);
    product.setProductCategories(new ArrayList<>());
    Mockito.when(productService.getProductByStoreIdAndProductCodeCached(Mockito.anyString(), Mockito.anyString()))
      .thenReturn(product);
    
    Category category = new Category();
    category.setCategoryCode(CATEGORY_CODE);
    Mockito.when(categoryRepostory.findByStoreIdAndCategoryCodeAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString()))
      .thenReturn(category);
    
    ProductCategory prdCategory = new ProductCategory();
    Mockito.when(prdCategoryRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString()))
      .thenReturn(Arrays.asList(prdCategory));
    
    Mockito.when(prdCategoryRepository.saveAll(Mockito.anyList())).thenReturn(null);
    
    service.movePrdCategoryByProductCode(STORE_ID, PRODUCT_CODE, CATEGORY_CODE);
    
    Mockito.verify(productService).getProductByStoreIdAndProductCodeCached(Mockito.anyString(), Mockito.anyString());
    Mockito.verify(productService).setProductCategoriesWithCategoriesCached(STORE_ID, product, false);
    Mockito.verify(categoryRepostory).findByStoreIdAndCategoryCodeAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString());
    Mockito.verify(prdCategoryRepository).findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString());
    Mockito.verify(prdCategoryRepository).saveAll(Mockito.anyList());
    Mockito.verify(applicationCacheServiceBean).evictProductCategoriesCacheByStoreIdAndProductId(STORE_ID, product.getId());
    Mockito.verify(domainEventPublisherService)
        .publishProductChangeCategory(Mockito.any(Product.class), Mockito.eq(null), Mockito.eq(false), Mockito.eq(false),
            Mockito.eq(true), Mockito.eq(false), Mockito.eq(new HashSet<>()));
  }
  
  @Test
  public void movePrdCategoryByProductCode_NullProductCategory_ThrowException() throws Exception {
    Product product = new Product();
    product.setId(ID);
    product.setProductCategories(new ArrayList<ProductCategory>());
    Mockito.when(productService.getProductByStoreIdAndProductCodeCached(Mockito.anyString(), Mockito.anyString()))
      .thenReturn(product);
    
    Category category = new Category();
    category.setCategoryCode(CATEGORY_CODE);
    Mockito.when(categoryRepostory.findByStoreIdAndCategoryCodeAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString()))
      .thenReturn(category);
    
    Mockito.when(prdCategoryRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString()))
      .thenReturn(null);
    
    try{
      service.movePrdCategoryByProductCode(STORE_ID, PRODUCT_CODE, CATEGORY_CODE);
    } catch(Exception e){
      Mockito.verify(productService).getProductByStoreIdAndProductCodeCached(Mockito.anyString(), Mockito.anyString());
      Mockito.verify(productService).setProductCategoriesWithCategoriesCached(STORE_ID, product, false);
      Mockito.verify(categoryRepostory).findByStoreIdAndCategoryCodeAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString());
      Mockito.verify(prdCategoryRepository).findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString());
    }
  }
  
  @Test
  public void movePrdCategoryByProductCode_NullCategory_ThrowException() throws Exception {
    product = new Product();
    product.setId(ID);
    product.setProductCategories(new ArrayList<ProductCategory>());
    Mockito.when(productService.getProductByStoreIdAndProductCodeCached(Mockito.anyString(), Mockito.anyString()))
      .thenReturn(product);
    
    Mockito.when(categoryRepostory.findByStoreIdAndCategoryCodeAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString()))
      .thenReturn(null);

    try{
      service.movePrdCategoryByProductCode(STORE_ID, PRODUCT_CODE, CATEGORY_CODE);
    } catch(Exception e){
      Mockito.verify(productService).getProductByStoreIdAndProductCodeCached(Mockito.anyString(), Mockito.anyString());
      Mockito.verify(productService).setProductCategoriesWithCategoriesCached(STORE_ID, product, false);
      Mockito.verify(categoryRepostory).findByStoreIdAndCategoryCodeAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString());
    }
  }
  
  @Test
  public void movePrdCategoryByProductCode_NullProduct_ThrowException() throws Exception {
    Mockito.when(productService.getProductByStoreIdAndProductCodeCached(Mockito.anyString(), Mockito.anyString()))
      .thenReturn(null);
    
    try{
      service.movePrdCategoryByProductCode(STORE_ID, PRODUCT_CODE, CATEGORY_CODE);
    } catch(Exception e){
      Mockito.verify(productService).getProductByStoreIdAndProductCodeCached(Mockito.anyString(), Mockito.anyString());
    }
  }
  
  @Test
  public void addProductAttributes_DescriptiveAttr_Success() throws Exception {
    product = new Product();
    product.setId(ID);
    Mockito.when(productRepository.findByStoreIdAndProductCode(Mockito.any(), Mockito.any()))
      .thenReturn(product);
    attr.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
    Mockito.when(attrRepository.findByStoreIdAndAttributeCode(Mockito.any(), Mockito.any()))
      .thenReturn(attr);
    Mockito.when(prdAttributeRepository.save(Mockito.any(ProductAttribute.class))).thenReturn(null);
    Mockito.when(prdAttrValueRepository.save(Mockito.any(ProductAttributeValue.class))).thenReturn(null);
    
    AddProductAttributesDTO request = new AddProductAttributesDTO();
    NewAttributeRequestDTO attrReq = new NewAttributeRequestDTO();
    NewAttributeRequestDTO attrReq2 = new NewAttributeRequestDTO();
    request.setNewAttributes(Arrays.asList(attrReq, attrReq2));
    service.addProductAttributes(STORE_ID, request);
    
    Mockito.verify(productRepository).findByStoreIdAndProductCode(Mockito.any(), Mockito.any());
    Mockito.verify(attrRepository, Mockito.atLeastOnce()).findByStoreIdAndAttributeCode(Mockito.any(), Mockito.any());
    Mockito.verify(prdAttributeRepository, Mockito.atLeastOnce()).save(Mockito.any(ProductAttribute.class));
    Mockito.verify(prdAttrValueRepository, Mockito.atLeastOnce()).save(Mockito.any(ProductAttributeValue.class));
    Mockito.verify(applicationCacheServiceBean).evictProductAttributesCacheByStoreIdAndProductId(STORE_ID, ID);
  }
  
  @Test
  public void addProductAttributes_PredefineAttr_Success() throws Exception {
    product = new Product();
    Mockito.when(productRepository.findByStoreIdAndProductCode(Mockito.any(), Mockito.any()))
      .thenReturn(product);
    attr.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    Mockito.when(attrRepository.findByStoreIdAndAttributeCode(Mockito.any(), Mockito.any()))
      .thenReturn(attr);
    Mockito.when(prdAttributeRepository.save(Mockito.any(ProductAttribute.class))).thenReturn(null);
    Mockito.when(predefAllowedAttrRepository.findByStoreIdAndAttributeAndValueAndMarkForDeleteFalse(
        Mockito.any(), (Attribute) Mockito.any(), Mockito.any())).thenReturn(prdAttrValue);
    Mockito.when(prdAttrValueRepository.save(Mockito.any(ProductAttributeValue.class))).thenReturn(null);
    
    AddProductAttributesDTO request = new AddProductAttributesDTO();
    NewAttributeRequestDTO attrReq = new NewAttributeRequestDTO();
    NewAttributeRequestDTO attrReq2 = new NewAttributeRequestDTO();
    request.setNewAttributes(Arrays.asList(attrReq, attrReq2));
    service.addProductAttributes(STORE_ID, request);
    
    Mockito.verify(productRepository).findByStoreIdAndProductCode(Mockito.any(), Mockito.any());
    Mockito.verify(attrRepository, Mockito.atLeastOnce()).findByStoreIdAndAttributeCode(Mockito.any(), Mockito.any());
    Mockito.verify(prdAttributeRepository, Mockito.atLeastOnce()).save(Mockito.any(ProductAttribute.class));
    Mockito.verify(predefAllowedAttrRepository, Mockito.atLeastOnce()).findByStoreIdAndAttributeAndValueAndMarkForDeleteFalse(
        Mockito.any(), (Attribute) Mockito.any(), Mockito.any());
    Mockito.verify(prdAttrValueRepository, Mockito.atLeastOnce()).save(Mockito.any(ProductAttributeValue.class));
    Mockito.verify(applicationCacheServiceBean).evictProductAttributesCacheByStoreIdAndProductId(STORE_ID, product.getId());
  }
  
  @Test
  public void addProductAttributes_InvalidAttrType_ThrowException() throws Exception {
    product = new Product();
    Mockito.when(productRepository.findByStoreIdAndProductCode(Mockito.anyString(), Mockito.anyString()))
      .thenReturn(product);
    
    try{
      AddProductAttributesDTO request = new AddProductAttributesDTO();
      NewAttributeRequestDTO attrReq = new NewAttributeRequestDTO();
      request.setNewAttributes(Arrays.asList(attrReq));
      service.addProductAttributes(null, request);
    } catch(Exception e){
      Mockito.verify(productRepository).findByStoreIdAndProductCode(Mockito.any(), Mockito.any());
    }
  }
  
  @Test
  public void addProductAttributes_DuplicateAttr_ThrowException() throws Exception {
    product = new Product();
    ProductAttribute prdAttr = new ProductAttribute();
    prdAttr.setAttribute(attr);
    product.setProductAttributes(Arrays.asList(prdAttr));
    Mockito.when(productRepository.findByStoreIdAndProductCode(Mockito.anyString(), Mockito.anyString()))
      .thenReturn(product);
    Mockito.when(attrRepository.findByStoreIdAndAttributeCode(Mockito.anyString(), Mockito.anyString()))
      .thenReturn(attr);
    Mockito.when(predefAllowedAttrRepository.findByStoreIdAndAttributeAndValueAndMarkForDeleteFalse(
        Mockito.anyString(), (Attribute) Mockito.any(), Mockito.anyString())).thenReturn(prdAttrValue);
    Mockito.when(prdAttrValueRepository.save(Mockito.any(ProductAttributeValue.class))).thenReturn(null);
    
    try{
      AddProductAttributesDTO request = new AddProductAttributesDTO();
      NewAttributeRequestDTO attrReq = new NewAttributeRequestDTO();
      attrReq.setAttributeCode("VALID-CODE");
      NewAttributeRequestDTO attrReq2 = new NewAttributeRequestDTO();
      attrReq2.setAttributeCode(ATTR_CODE);
      request.setNewAttributes(Arrays.asList(attrReq, attrReq2));
      service.addProductAttributes(null, request);
    } catch(Exception e){
      Mockito.verify(productRepository).findByStoreIdAndProductCode(Mockito.any(), Mockito.any());
    }
  }
  
  @Test
  public void addProductAttributes_PredefAllowedNotFound_ThrowException() throws Exception {
    product = new Product();
    Mockito.when(productRepository.findByStoreIdAndProductCode(Mockito.anyString(), Mockito.anyString()))
      .thenReturn(product);
    attr.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
    Mockito.when(attrRepository.findByStoreIdAndAttributeCode(Mockito.anyString(), Mockito.anyString()))
      .thenReturn(attr);
    Mockito.when(prdAttributeRepository.save(Mockito.any(ProductAttribute.class))).thenReturn(null);
    Mockito.when(predefAllowedAttrRepository.findByStoreIdAndAttributeAndValueAndMarkForDeleteFalse(
        Mockito.anyString(), (Attribute) Mockito.any(), Mockito.anyString())).thenReturn(null);
    try{
      AddProductAttributesDTO request = new AddProductAttributesDTO();
      NewAttributeRequestDTO attrReq = new NewAttributeRequestDTO();
      request.setNewAttributes(Arrays.asList(attrReq));
      service.addProductAttributes(null, request);
    } catch(Exception e){
      Mockito.verify(productRepository).findByStoreIdAndProductCode(Mockito.any(), Mockito.any());
    }
  }
  
  @Test
  public void addProductAttributes_AttrNotFound_ThrowException() throws Exception {
    product = new Product();
    Mockito.when(productRepository.findByStoreIdAndProductCode(Mockito.anyString(), Mockito.anyString()))
      .thenReturn(product);
    Mockito.when(attrRepository.findByStoreIdAndAttributeCode(Mockito.anyString(), Mockito.anyString()))
      .thenReturn(null);
    try{
      AddProductAttributesDTO request = new AddProductAttributesDTO();
      NewAttributeRequestDTO attrReq = new NewAttributeRequestDTO();
      request.setNewAttributes(Arrays.asList(attrReq));
      service.addProductAttributes(null, request);
    } catch(Exception e){
      Mockito.verify(productRepository).findByStoreIdAndProductCode(Mockito.any(), Mockito.any());
    }
  }

  @Test
  public void getMasterParentCategoryByProductCodeTest() throws Exception{
    Mockito.when(productService
        .getProductByStoreIdAndProductCodeCached(Mockito.eq(STORE_ID), Mockito.eq(PRODUCT_CODE)))
        .thenReturn(product);
    Mockito.when(this.categoryService
        .findCategoryHierarchyByCategoryCode(Mockito.eq(STORE_ID), Mockito.anyString()))
        .thenReturn(Collections.singletonList(productCategory.getCategory()));
    List<Category> result =
        this.service.getMasterParentCategoryByProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productService)
        .getProductByStoreIdAndProductCodeCached(Mockito.eq(STORE_ID), Mockito.eq(PRODUCT_CODE));
    Mockito.verify(this.categoryService)
        .findCategoryHierarchyByCategoryCode(Mockito.eq(STORE_ID), Mockito.anyString());
    Mockito.verify(productService).setProductCategoriesWithCategoriesCached(STORE_ID, product, false);
    Assertions.assertNotNull(result);
    Assertions.assertEquals(productCategory.getCategory(), result.get(0));
  }

  @Test
  public void getMasterParentCategoryByProductCode_whenProductNotExistTest() {
    List<Category> result = null;
    Mockito.when(productService
        .getProductByStoreIdAndProductCodeCached(Mockito.eq(STORE_ID), Mockito.eq(PRODUCT_CODE)))
        .thenReturn(null);
    try {
      result = this.service.getMasterParentCategoryByProductCode(STORE_ID, PRODUCT_CODE);
    } catch (ApplicationRuntimeException ex) {
      Mockito.verify(productService)
          .getProductByStoreIdAndProductCodeCached(Mockito.eq(STORE_ID), Mockito.eq(PRODUCT_CODE));
      Assertions.assertNull(result);

    }
  }

  @Test
  public void getMasterParentCategoryByProductCode_whenEmptyListTest() {

    product.setProductCategories(null);
    Mockito.when(productService
        .getProductByStoreIdAndProductCodeCached(Mockito.eq(STORE_ID), Mockito.eq(PRODUCT_CODE)))
        .thenReturn(product);
    List<Category> result =
        this.service.getMasterParentCategoryByProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productService)
        .getProductByStoreIdAndProductCodeCached(Mockito.eq(STORE_ID), Mockito.eq(PRODUCT_CODE));
    Mockito.verify(productService).setProductCategoriesWithCategoriesCached(STORE_ID, product, false);
    Assertions.assertTrue(CollectionUtils.isEmpty(result));
  }

  @Test
  public void getMasterParentCategoryByProductCode_whenCatalogCodeNotMatchTest()
      throws Exception {
    product.getProductCategories().get(0).getCategory().getCatalog().setCatalogCode("12512");
    Mockito.when(productService
        .getProductByStoreIdAndProductCodeCached(Mockito.eq(STORE_ID), Mockito.eq(PRODUCT_CODE)))
        .thenReturn(product);
    Mockito.when(categoryService.findCategoryHierarchyByCategoryCode(STORE_ID, productCategory.getCategory().getCategoryCode())).thenReturn(
        Collections.singletonList(category));
    List<Category> result =
        this.service.getMasterParentCategoryByProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productService)
        .getProductByStoreIdAndProductCodeCached(Mockito.eq(STORE_ID), Mockito.eq(PRODUCT_CODE));
    Mockito.verify(productService).setProductCategoriesWithCategoriesCached(STORE_ID, product, false);
    Assertions.assertTrue(CollectionUtils.isEmpty(result));
  }

  @Test
  public void getMasterParentCategoryByProductCodeExceptionTest() throws Exception {
    Mockito.when(productService
        .getProductByStoreIdAndProductCodeCached(Mockito.eq(STORE_ID), Mockito.eq(PRODUCT_CODE)))
        .thenReturn(product);
    Mockito.doThrow(new RuntimeException()).when(this.categoryService)
        .findCategoryHierarchyByCategoryCode(Mockito.eq(STORE_ID), Mockito.anyString());
    List<Category> result =
        this.service.getMasterParentCategoryByProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productService)
        .getProductByStoreIdAndProductCodeCached(Mockito.eq(STORE_ID), Mockito.eq(PRODUCT_CODE));
    Mockito.verify(productService).setProductCategoriesWithCategoriesCached(STORE_ID, product, false);
    Mockito.verify(this.categoryService)
        .findCategoryHierarchyByCategoryCode(Mockito.eq(STORE_ID), Mockito.anyString());
    Assertions.assertNull(result.get(0));
  }

  @Test
  public void findCategoryCountByProductIdInAndMarkForDeleteFalseTest() throws Exception {
    Mockito.when(repository.findCategoryCountByProductIdInAndMarkForDeleteFalse(Arrays.asList(ID))).thenReturn(objects);
    List<Object[]> result = this.service.findCategoryCountByProductIdInAndMarkForDeleteFalse(Arrays.asList(ID));
    Mockito.verify(repository).findCategoryCountByProductIdInAndMarkForDeleteFalse(Arrays.asList(ID));
    Assertions.assertNotNull(result);
    Assertions.assertEquals(1, result.size());
  }


  @Test
  public void findByProductIdInAndCategoryIdInAndMarkForDeleteFalseTest() throws Exception {
    Mockito.when(
        repository.findByProductIdInAndCategoryIdInAndMarkForDeleteFalse(Arrays.asList(ID), Arrays.asList(CATEGORY_ID)))
        .thenReturn(Arrays.asList(ID));
    List<String> result = this.service
        .findByProductIdInAndCategoryIdInAndMarkForDeleteFalse(Arrays.asList(ID), Arrays.asList(CATEGORY_ID));
    Mockito.verify(repository)
        .findByProductIdInAndCategoryIdInAndMarkForDeleteFalse(Arrays.asList(ID), Arrays.asList(CATEGORY_ID));
    Assertions.assertNotNull(result);
    Assertions.assertEquals(ID, result.get(0));
  }

  @Test
  public void getProductCategoriesByStoreIdAndProductIdTest() {
    Mockito.when(repository.findByStoreIdAndProductId(STORE_ID, ID))
        .thenReturn(Collections.singletonList(productCategory));
    service.getProductCategoriesByStoreIdAndProductIdCached(STORE_ID, ID);
    Mockito.verify(repository).findByStoreIdAndProductId(STORE_ID, ID);
  }

  @Test
  void productCategoryDeletionsById() {
    List<String> productCategoryIds = List.of("category1", "category2");
    service.deleteByProductCategoryIds(productCategoryIds);
    Mockito.verify(repository).deleteAllById(productCategoryIds);
  }
}
