package com.gdn.x.productcategorybase.service.impl;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Stream;

import org.apache.commons.lang3.StringUtils;
import org.hamcrest.Matchers;
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
import com.gdn.common.util.BeanUtils;
import org.springframework.context.ApplicationContext;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.util.GdnUUIDHelper;
import com.gdn.x.productcategorybase.AttributeType;
import com.gdn.x.productcategorybase.CatalogType;
import com.gdn.x.productcategorybase.CategoryChangeEventType;
import com.gdn.x.productcategorybase.Constants;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.customrepository.CategoryDAO;
import com.gdn.x.productcategorybase.dto.CategoryConfigurationDTO;
import com.gdn.x.productcategorybase.dto.CategoryTreeDTO;
import com.gdn.x.productcategorybase.dto.CategoryTreeNodeDTO;
import com.gdn.x.productcategorybase.dto.CustomCategoryDto;
import com.gdn.x.productcategorybase.dto.mail.MailRecipientRequest;
import com.gdn.x.productcategorybase.dto.request.OriginalSalesCategoryRequest;
import com.gdn.x.productcategorybase.dto.response.CategoryTreeNodeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryTreeResponse;
import com.gdn.x.productcategorybase.entity.Attribute;
import com.gdn.x.productcategorybase.entity.Catalog;
import com.gdn.x.productcategorybase.entity.Category;
import com.gdn.x.productcategorybase.entity.CategoryAttribute;
import com.gdn.x.productcategorybase.entity.CategoryConfiguration;
import com.gdn.x.productcategorybase.entity.CategoryReference;
import com.gdn.x.productcategorybase.entity.CategoryShipping;
import com.gdn.x.productcategorybase.entity.OriginalSalesCategory;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductCategory;
import com.gdn.x.productcategorybase.repository.CatalogRepository;
import com.gdn.x.productcategorybase.repository.CategoryAttributeRepository;
import com.gdn.x.productcategorybase.repository.CategoryConfigurationRepository;
import com.gdn.x.productcategorybase.repository.CategoryReferenceRepository;
import com.gdn.x.productcategorybase.repository.CategoryRepository;
import com.gdn.x.productcategorybase.repository.ProductCategoryRepository;
import com.gdn.x.productcategorybase.service.CatalogService;
import com.gdn.x.productcategorybase.service.CategoryService;
import com.gdn.x.productcategorybase.service.CategoryShippingService;
import com.gdn.x.productcategorybase.service.DomainEventPublisherService;
import com.gdn.x.productcategorybase.service.OriginalSalesCategoryService;
import com.gdn.x.productcategorybase.service.mailservice.MailDeliveryService;

public class CategoryServiceTest {

  private static final String ID = "id";
  private static final String STORE_ID = "10001";
  private static final String CATEGORY_ID = "3";
  private static final String CATEGORY_CODE = "CA-1234";
  private static final String CATEGORY_CODE_2 = "CA-1234-2";
  private static final String ATTRIBUTE_CODE = "attributeCode";
  private static final String CATEGORY_ID2 = "4";
  private static final String CATEGORY_ID1 = "1";
  private static final String CATEGORY_ID3 = "2";
  private static final String CATEGORY1_NAME = "CATEGORY_1_NAME";
  private static final String CATEGORY1_CHILD1_NAME = "CATEGORY_1_CHILD_1_NAME";
  private static final String CATEGORY1_CHILD2_NAME = "CATEGORY_1_CHILD_2_NAME";
  private static final String CATEGORY1_CHILD3_NAME = "CATEGORY_1_CHILD_3_NAME";
  private static final String CATEGORY2_NAME = "CATEGORY_2_NAME";
  private static final Pageable DEFAULT_PAGEABLE = PageRequest.of(0, 10);
  private static final String CATEGORY1_CODE = "CATEGORY_1_CODE";
  private static final String CATEGORY2_CODE = "CATEGORY_2_CODE";
  private static final String CATEGORY3_CODE = "CATEGORY_3_CODE";
  private static final String CATEGORY4_CODE = "CATEGORY_4_CODE";
  private static final String DEFAULT_STATE = "ALL";
  private static final String ACTIVE_STATE = "ACTIVE";
  private static final String INACTIVE_STATE = "INACTIVE";
  private static final String PARENT_CATEGORY_NAME = "parent-category-name";
  private static final String PARENT_CATEGORY_ID = "parent-category-id";
  private static final String CATALOG_NAME = "catelogName";
  private static final String NON_INVENTORY_CODE = "nonInventoryCode";
  private static final String CATALOG_ID = "10001";
  private static final Boolean DISPLAY = true;
  private static final String CATEGORY_UPDATE_NOTIFICATION_TEMPLATE =
      "PCB_CATEGORY_UPDATE_NOTIFICATION_TEMPLATE";
  private static final String SENDER="ABC@abc.com";
  private static final String MESSAGE_IDENTIFIER_KEY="userName";
  private static final long COUNT = 1;
  private static final String MASTER_CATALOG_ID = "C-001";
  private static final String MASTER_CATALOG = "MASTER_CATALOG";
  private static final String DOCUMENTS = "Doctor's prescription, Passport, Driving License";
  private static final String ALL = "ALL";
  private static final String DOCUMENT_REQUIRED = "documentRequired";
  private static final String DOCUMENT_NOT_REQUIRED = "documentNotRequired";

  @Mock
  private CategoryRepository categoryRepository;
  @Mock
  private ApplicationCacheServiceBean applicationCacheServiceBean;
  @Mock
  private CategoryDAO categoryDAO;
  @Mock
  private CategoryAttributeRepository categoryAttributeRepository;
  @Mock
  private CategoryReferenceRepository categoryReferenceRepository;
  @Mock
  private ProductCategoryRepository productCategoryRepository;
  @Mock
  private Page<Category> categoryPage;
  @Mock
  private DomainEventPublisherService domainEventPublisherService;
  @Mock
  private CatalogRepository catalogRepository;
  @Mock
  private CatalogService catalogService;
  @Mock
  private MailDeliveryService mailDeliveryService;
  @Mock
  private CategoryShippingService categoryShippingService;
  @Mock
  private CategoryConfigurationRepository categoryConfigurationRepository;
  @InjectMocks
  private CategoryServiceBean categoryServiceBean;
  @Mock
  private OriginalSalesCategoryService originalSalesCategoryService;
  @Captor
  private ArgumentCaptor<Category> categoryArgumentCaptor;

  @Mock
  private ApplicationContext applicationContext;

  @Mock
  private CategoryService categoryService;

  @Mock
  private CacheServiceHelperBean cacheServiceHelperBean;
  private Category category;
  private CategoryReference categoryReference;
  private Pageable defaultPageable;
  private List<Catalog> catalogList;
  private List<Object[]> catList;
  private List<CategoryTreeDTO> catDTOList;
  private List<CategoryTreeDTO> categoryTreeDTOList;
  private List<String> categoryCodes;
  private List<String> categoryIds;
  private List<Category> categories;
  private Page<Category> getCategoryPage;
  List<CategoryTreeDTO> categoryTreeDTOS = new ArrayList<>();
  Category category1 = new Category();
  Category category2 = new Category();
  Category category3 = new Category();
  Category category4 = new Category();
  CategoryTreeDTO categoryTreeDTO1 = new CategoryTreeDTO();
  CategoryTreeDTO categoryTreeDTO2 = new CategoryTreeDTO();
  CategoryTreeDTO categoryTreeDTO3 = new CategoryTreeDTO();
  CategoryTreeDTO categoryTreeDTO4 = new CategoryTreeDTO();
  CategoryAttribute categoryAttribute = new CategoryAttribute();
  List<CategoryAttribute> categoryAttributes= new ArrayList<>();
  private CategoryTreeNodeDTO c1;
  private CategoryTreeNodeDTO c2;
  private CategoryTreeNodeDTO c3;
  private CategoryTreeNodeDTO c4;
  private CategoryConfiguration categoryConfiguration = new CategoryConfiguration();
  private CategoryConfigurationDTO categoryConfigurationDTO = new CategoryConfigurationDTO();
  private List<CategoryTreeNodeDTO> categoryTreeNodeDTOList = new ArrayList<>();

  private List<Category> getDefaultCategoriesWithCategoryCode() {
    List<Category> categories = new ArrayList<Category>();
    Category cat1 = new Category(STORE_ID, CATEGORY1_CODE, CATEGORY1_NAME);
    cat1.setId("1");
    Category cat2 = new Category(STORE_ID, CATEGORY2_CODE, CATEGORY2_NAME);
    cat2.setId("2");
    categories.add(cat1);
    categories.add(cat2);
    return categories;
  }

  private List<Object[]> getCategoriesWithCategoryCode() {
    List<Object[]> categories = new ArrayList<Object[]>();
    Object[] object1 = new Object[] {"1", CATEGORY1_CODE};
    Object[] object2 = new Object[] {"2", CATEGORY2_CODE};
    categories.add(object1);
    categories.add(object2);
    return categories;
  }

  protected List<Category> getDefaultCategories() {
    List<Category> categories = new ArrayList<Category>();
    for (int i = 1; i <= 3; i++) {
      this.getDefaultCategory(i);
    }
    return categories;
  }

  private Category getDefaultCategory(int sequence) {
    return new Category(CategoryServiceTest.STORE_ID, CategoryServiceTest.CATEGORY1_NAME, sequence);
  }

  @BeforeEach
  public void initialize() throws Exception {
    MockitoAnnotations.initMocks(this);
    ReflectionTestUtils.setField(categoryServiceBean, "mailSender", "ABC@abc.com");

    this.category =
        new Category(CategoryServiceTest.STORE_ID, CategoryServiceTest.CATEGORY1_NAME, 1);

    Category masterCategory = new Category(CategoryServiceTest.STORE_ID, "master-category", 1);
    Category salesCategory = new Category(CategoryServiceTest.STORE_ID, "sales-category", 1);
    this.categoryReference = new CategoryReference(masterCategory, salesCategory);
    this.categoryReference.setId(CATEGORY_ID);

    this.defaultPageable = PageRequest.of(0, 10);
    Mockito.when(this.domainEventPublisherService.publishCategory((Category) any(), Mockito.anyList(),
        Mockito.anySet(), Mockito.anyBoolean()))
        .thenReturn(null);

    Mockito
        .when(
            this.categoryRepository.findByStoreIdAndCategoryNameAndCatalogTypeAndMarkForDeleteFalse(
                Mockito.anyString(), Mockito.anyString(), (CatalogType) any(),
                (Pageable) any()))
        .thenReturn(new PageImpl<Category>(new ArrayList<Category>()));

    this.catalogList = new ArrayList<>();
    Catalog catalog = new Catalog(MASTER_CATALOG_ID);
    this.catalogList.add(catalog);

    this.catList = new ArrayList<>();
    this.catList.add(new Object[] {"1", "A-01", "Cat 1", ""});
    this.catList.add(new Object[] {"2", "A-01-01", "Cat 1", "1"});
    this.catList.add(new Object[] {"3", "A-01-02", "Cat 1", "1"});
    this.catList.add(new Object[] {"4", "A-01-02", "Cat 1", "1"});
    this.catList.add(new Object[] {"5", "A-01-01-01", "Cat 1", "2"});
    this.catList.add(new Object[] {"6", "A-01-01-02", "Cat 1", "2"});
    this.catList.add(new Object[] {"21", "B-02-01", "Cat 2", null});

    this.catDTOList = new ArrayList<>();
    this.catDTOList.add(new CategoryTreeDTO("1", "A-01", "Cat 1", "", "", true, "Resep Dokter,KTP,Passport"));
    this.catDTOList.add(new CategoryTreeDTO("2", "A-01-01", "Cat 1", "1", "", true, null));
    this.catDTOList.add(new CategoryTreeDTO("3", "A-01-02", "Cat 1", "1", "", true, "Driving license1"));
    this.catDTOList.add(new CategoryTreeDTO("4", "A-01-02", "Cat 1", "1", "", true, StringUtils.EMPTY));
    this.catDTOList.add(new CategoryTreeDTO("5", "A-01-01-01", "Cat 1", "2"));
    this.catDTOList.add(new CategoryTreeDTO("6", "A-01-01-02", "Cat 1", "2"));
    this.catDTOList.add(new CategoryTreeDTO("21", "B-02-01", "Cat 2", ""));

    this.categoryTreeDTOList = new ArrayList<>();
    this.categoryTreeDTOList.add(new CategoryTreeDTO("1", "A-01", "Cat 1", "Cat 1 English", "", false));
    this.categoryTreeDTOList.add(new CategoryTreeDTO("2", "A-01-01", "Cat 1", "Cat 1 English", "1", false));
    this.categoryTreeDTOList.add(new CategoryTreeDTO("3", "A-01-02", "Cat 1", "Cat 1 English", "2", true));
    this.categoryTreeDTOList.add(new CategoryTreeDTO("4", "A-01-02", "Cat 1", "Cat 1 English", "1", true));
    this.categoryTreeDTOList.add(new CategoryTreeDTO("41", "A-01-02", "Cat 1", "Cat 1 English", "4", false));
    this.categoryTreeDTOList.add(new CategoryTreeDTO("5", "A-01-01-01", "Cat 1", "Cat 1 English", "2", false));
    this.categoryTreeDTOList.add(new CategoryTreeDTO("6", "A-01-01-02", "Cat 1", "Cat 1 English", "2", true));
    this.categoryTreeDTOList.add(new CategoryTreeDTO("21", "B-02-01", "Cat 2", "Cat 1 English", "", true));
    this.categoryTreeDTOList.add(new CategoryTreeDTO("7", "A-01-03", "Cat 3", "Cat 3 English", "6", false));
    this.categoryTreeDTOList.add(new CategoryTreeDTO("8", "A-01-04", "Cat 4", "Cat 4 English", "7", true));
    this.categoryTreeDTOList.add(new CategoryTreeDTO("9", "A-01-05", "Cat 5", "Cat 5 English", "7", true));
    this.categoryTreeDTOList.add(new CategoryTreeDTO("10", "A-01-06", "Cat 6", "Cat 6 English", "7", false));

    this.categoryCodes = new ArrayList<>();
    this.categoryCodes.add(CATEGORY_CODE);
    this.categoryCodes.add(CATEGORY1_CODE);

    this.categoryIds = new ArrayList<>();
    this.categoryIds.add(CATEGORY_CODE);
    categories = this.getDefaultCategories();
    getCategoryPage =
        new PageImpl<Category>(categories, CategoryServiceTest.DEFAULT_PAGEABLE, categories.size());
    initializeCategoriesPublisherMock();
    c1 = new CategoryTreeNodeDTO("id1", "c1", "cat1", null);
    c2 = new CategoryTreeNodeDTO("id2", "c2", "cat2", null);
    c3 = new CategoryTreeNodeDTO("id3", "c3", "cat3", "id1");
    c4 = new CategoryTreeNodeDTO("id4", "c4", "cat4", "id1");
    categoryConfiguration.setMarkForDelete(false);
    categoryConfiguration.setCategory(category1);
    categoryConfiguration.setReviewConfig(Constants.PRE_LIVE_STATUS);
    categoryConfiguration.getCategory().setCategoryCode(CATEGORY_CODE);
    categoryConfiguration.setId(ID);
    categoryConfiguration.setCategoryId(CATEGORY_ID1);
    BeanUtils.copyProperties(categoryConfiguration, categoryConfigurationDTO);
    Mockito.when(applicationContext.getBean(CategoryService.class)).thenReturn(categoryService);
  }

  @AfterEach
  public void postTest() {
    verifyNoMoreInteractions(this.categoryRepository);
    verifyNoMoreInteractions(this.categoryAttributeRepository);
    verifyNoMoreInteractions(this.productCategoryRepository, this.mailDeliveryService);
    verifyNoMoreInteractions(this.categoryShippingService);
    verifyNoMoreInteractions(this.applicationContext);
    verifyNoMoreInteractions(this.originalSalesCategoryService);
    verifyNoMoreInteractions(this.cacheServiceHelperBean);
  }

  @Test
  public void testAdjustCategoryAttributeExists() throws Exception {
    Category oldCategory =
        new Category(CategoryServiceTest.STORE_ID, CategoryServiceTest.CATEGORY1_NAME, 1); // sequence
    // 1
    Category newCategory =
        new Category(CategoryServiceTest.STORE_ID, CategoryServiceTest.CATEGORY2_NAME, 2); // name
    // =
    // category2_name,
    // sequence 2
    Category parentCategory =
        new Category(STORE_ID, PARENT_CATEGORY_NAME, 2); // name
    parentCategory.setId(PARENT_CATEGORY_ID);
    oldCategory.setId(CategoryServiceTest.CATEGORY_ID);
    newCategory.setId(CategoryServiceTest.CATEGORY_ID2);
    newCategory.setParentCategory(parentCategory);

    List<CategoryReference> masterCategoryReferences = new ArrayList<CategoryReference>();
    masterCategoryReferences.add(this.categoryReference);

    oldCategory.setMasterCategoryReferences(masterCategoryReferences);
    newCategory.setMasterCategoryReferences(masterCategoryReferences);

    CategoryAttribute oldCategoryAttribute = new CategoryAttribute();
    oldCategoryAttribute.setId(CategoryServiceTest.CATEGORY_ID);

    CategoryAttribute newCategoryAttribute = new CategoryAttribute();
    newCategoryAttribute.setId(CategoryServiceTest.CATEGORY_ID2);

    List<CategoryAttribute> listOldCategoryAttributes = new ArrayList<CategoryAttribute>();
    listOldCategoryAttributes.add(oldCategoryAttribute);
    oldCategory.setCategoryAttributes(listOldCategoryAttributes);

    newCategory.setCategoryAttributes(listOldCategoryAttributes);

    Mockito.when(this.categoryRepository.findById(CategoryServiceTest.CATEGORY_ID))
        .thenReturn(Optional.of(oldCategory));
    Mockito.when(this.categoryRepository.findById(CategoryServiceTest.CATEGORY_ID2))
        .thenReturn(Optional.of(newCategory));
    Mockito.when(applicationContext.getBean(CategoryService.class)).thenReturn(categoryService);

    this.categoryServiceBean.adjustCategory(oldCategory, newCategory,
        Mockito.anyString(), null);

    verify(this.categoryRepository, times(1)).findById(CategoryServiceTest.CATEGORY_ID);
    verify(this.categoryRepository, times(1)).saveAndFlush(oldCategory);
    verify(this.domainEventPublisherService)
        .publishCategory(oldCategory, Arrays.asList(CategoryChangeEventType.DATA_CHANGE), new HashSet<>(), false);
    verify(categoryRepository).findByStoreIdAndParentCategoryIdAndMarkForDeleteFalse(STORE_ID, CATEGORY_ID);
    assertFalse(oldCategoryAttribute.isMarkForDelete());
  }

  @Test
  public void testAdjustCategoryAttributeNotExist() throws Exception {
    Category oldCategory =
        new Category(CategoryServiceTest.STORE_ID, CategoryServiceTest.CATEGORY1_NAME, 1);
    Category newCategory =
        new Category(CategoryServiceTest.STORE_ID, CategoryServiceTest.CATEGORY2_NAME, 2);
    oldCategory.setId(CategoryServiceTest.CATEGORY_ID);
    newCategory.setId(CategoryServiceTest.CATEGORY_ID2);

    CategoryAttribute oldCategoryAttribute = new CategoryAttribute();
    oldCategoryAttribute.setId(CategoryServiceTest.CATEGORY_ID);

    CategoryAttribute newCategoryAttribute = new CategoryAttribute();

    List<CategoryAttribute> listOldCategoryAttributes = new ArrayList<CategoryAttribute>();
    listOldCategoryAttributes.add(oldCategoryAttribute);
    oldCategory.setCategoryAttributes(listOldCategoryAttributes);

    List<CategoryAttribute> listNewCategoryAttributes = new ArrayList<CategoryAttribute>();
    listNewCategoryAttributes.add(newCategoryAttribute);
    newCategory.setCategoryAttributes(listNewCategoryAttributes);
    newCategory.setUpdatedDate(new Date());

    Mockito.when(this.categoryRepository.findById(CategoryServiceTest.CATEGORY_ID))
        .thenReturn(Optional.of(oldCategory));
    Mockito.when(this.categoryRepository.findById(CategoryServiceTest.CATEGORY_ID2))
        .thenReturn(Optional.of(newCategory));
    Mockito.when(applicationContext.getBean(CategoryService.class)).thenReturn(categoryService);

    this.categoryServiceBean.adjustCategory(oldCategory, newCategory,
        Mockito.anyString(), 4);

    assertTrue(oldCategoryAttribute.isMarkForDelete());

    verify(this.categoryRepository, times(1)).findById(CategoryServiceTest.CATEGORY_ID);
    verify(this.categoryRepository, times(1)).saveAndFlush(oldCategory);

    assertEquals(newCategoryAttribute.getCategory(), oldCategory);

    int size = oldCategory.getCategoryAttributes().size();
    verify(this.domainEventPublisherService)
        .publishCategory((Category) any(), eq(Arrays.asList(CategoryChangeEventType.DATA_CHANGE)),
            Mockito.anySet(), eq(false));
    verify(this.mailDeliveryService)
        .sendMail(eq(CATEGORY_UPDATE_NOTIFICATION_TEMPLATE), eq(SENDER),
            Mockito.any(), any(Map.class), eq(MESSAGE_IDENTIFIER_KEY),
            Mockito.any(), any(MailRecipientRequest.class));

    assertEquals(oldCategory.getCategoryAttributes().get(size - 1).getCategory(), oldCategory);
    verify(categoryRepository).findByStoreIdAndParentCategoryIdAndMarkForDeleteFalse(STORE_ID, CATEGORY_ID);
  }

  @Test
  public void testAdjustCategoryAttributeExistsNotSame() throws Exception {
    Category oldCategory =
        new Category(CategoryServiceTest.STORE_ID, CategoryServiceTest.CATEGORY1_NAME, 1); // sequence
    // 1
    Category newCategory =
        new Category(CategoryServiceTest.STORE_ID, CategoryServiceTest.CATEGORY2_NAME, 2); // name
    // =
    // category2_name,
    // sequence 2
    oldCategory.setId(CategoryServiceTest.CATEGORY_ID);
    newCategory.setId(CategoryServiceTest.CATEGORY_ID2);

    List<CategoryReference> masterCategoryReferences = new ArrayList<CategoryReference>();
    CategoryReference categoryReference2 = new CategoryReference();
    categoryReference2.setId(null);
    categoryReference2.setMasterCategory(new Category());
    masterCategoryReferences.add(categoryReference2);
    masterCategoryReferences.get(0).getMasterCategory().setId(CategoryServiceTest.CATEGORY_ID);

    List<CategoryReference> masterCategoryReferences2 = new ArrayList<CategoryReference>();
    this.categoryReference.getMasterCategory().setId(CATEGORY_ID);
    masterCategoryReferences2.add(this.categoryReference);
    oldCategory.setMasterCategoryReferences(masterCategoryReferences2);
    newCategory.setMasterCategoryReferences(masterCategoryReferences);

    CategoryAttribute oldCategoryAttribute = new CategoryAttribute();
    oldCategoryAttribute.setId(CategoryServiceTest.CATEGORY_ID);

    CategoryAttribute newCategoryAttribute = new CategoryAttribute();
    newCategoryAttribute.setId(CategoryServiceTest.CATEGORY_ID2);

    List<CategoryAttribute> listOldCategoryAttributes = new ArrayList<CategoryAttribute>();
    listOldCategoryAttributes.add(oldCategoryAttribute);
    oldCategory.setCategoryAttributes(listOldCategoryAttributes);

    newCategory.setCategoryAttributes(listOldCategoryAttributes);

    Mockito.when(this.categoryRepository.findById(CategoryServiceTest.CATEGORY_ID))
        .thenReturn(Optional.of(oldCategory));
    Mockito.when(this.categoryRepository.findById(CategoryServiceTest.CATEGORY_ID2))
        .thenReturn(Optional.of(newCategory));
    Mockito.when(applicationContext.getBean(CategoryService.class)).thenReturn(categoryService);

    this.categoryServiceBean.adjustCategory(oldCategory, newCategory,
        Mockito.anyString(), null);

    verify(this.categoryRepository, times(1)).findById(CategoryServiceTest.CATEGORY_ID);
    verify(this.categoryRepository, times(1)).saveAndFlush(oldCategory);
    verify(this.domainEventPublisherService)
        .publishCategory((Category) any(), eq(Arrays.asList(CategoryChangeEventType.DATA_CHANGE)),
            Mockito.anySet(), eq(false));
    verify(categoryRepository).findByStoreIdAndParentCategoryIdAndMarkForDeleteFalse(STORE_ID, CATEGORY_ID);
    assertFalse(oldCategoryAttribute.isMarkForDelete());
  }

  @Test
  public void testAdjustCategoryAttributeExistsNotSame2() throws Exception {
    Category oldCategory =
        new Category(CategoryServiceTest.STORE_ID, CategoryServiceTest.CATEGORY1_NAME, 1); // sequence
    // 1
    Category newCategory =
        new Category(CategoryServiceTest.STORE_ID, CategoryServiceTest.CATEGORY2_NAME, 2); // name
    oldCategory.setId(CategoryServiceTest.CATEGORY_ID);
    newCategory.setId(CategoryServiceTest.CATEGORY_ID2);

    List<CategoryReference> masterCategoryReferences = new ArrayList<CategoryReference>();
    CategoryReference categoryReference2 = new CategoryReference();
    categoryReference2.setId(null);
    categoryReference2.setMasterCategory(new Category());
    masterCategoryReferences.add(categoryReference2);
    masterCategoryReferences.get(0).getMasterCategory().setId(CategoryServiceTest.CATEGORY_ID2);

    List<CategoryReference> masterCategoryReferences2 = new ArrayList<CategoryReference>();
    this.categoryReference.getMasterCategory().setId(CATEGORY_ID);
    masterCategoryReferences2.add(this.categoryReference);
    oldCategory.setMasterCategoryReferences(masterCategoryReferences2);
    newCategory.setMasterCategoryReferences(masterCategoryReferences);

    CategoryAttribute oldCategoryAttribute = new CategoryAttribute();
    oldCategoryAttribute.setId(CategoryServiceTest.CATEGORY_ID);

    CategoryAttribute newCategoryAttribute = new CategoryAttribute();
    newCategoryAttribute.setId(CategoryServiceTest.CATEGORY_ID2);

    List<CategoryAttribute> listOldCategoryAttributes = new ArrayList<CategoryAttribute>();
    listOldCategoryAttributes.add(oldCategoryAttribute);
    oldCategory.setCategoryAttributes(listOldCategoryAttributes);

    newCategory.setCategoryAttributes(listOldCategoryAttributes);

    Mockito.when(this.categoryRepository.findById(CategoryServiceTest.CATEGORY_ID))
        .thenReturn(Optional.of(oldCategory));
    Mockito.when(this.categoryRepository.findById(CategoryServiceTest.CATEGORY_ID2))
        .thenReturn(Optional.of(newCategory));
    Mockito.doNothing().when(this.categoryRepository).setParentCategoryId(Mockito.anyString(),
        Mockito.anyString());

    this.categoryServiceBean.adjustCategory(oldCategory, newCategory, CATEGORY1_CHILD1_NAME, null);

    verify(this.categoryRepository, times(1)).findById(CategoryServiceTest.CATEGORY_ID);
    verify(this.categoryRepository, times(1)).saveAndFlush(oldCategory);
    verify(this.categoryRepository, times(1)).setParentCategoryId(Mockito.anyString(),
        Mockito.anyString());
    verify(this.domainEventPublisherService)
        .publishCategory((Category) any(), eq(Arrays.asList(CategoryChangeEventType.DATA_CHANGE)),
            Mockito.anySet(), eq(false));
    assertFalse(oldCategoryAttribute.isMarkForDelete());
    verify(categoryRepository).findByStoreIdAndParentCategoryIdAndMarkForDeleteFalse(STORE_ID, CATEGORY_ID);
  }

  @Test
  public void testFindById() throws Exception {
    String id = GdnUUIDHelper.generateUUID();
    Mockito.when(this.categoryRepository.findById(id)).thenReturn(Optional.of(this.category));
    Category savedCategory = this.categoryServiceBean.findById(id);
    Assertions.assertEquals(savedCategory, (this.category));
    verify(this.categoryRepository, times(1)).findById(id);
  }

  @Test
  public void testFindByName_ForStateIsActiveAndDocumentNotNull() {
    category = this.getDefaultCategory(1);
    category.setDocumentType(DOCUMENTS);
    categories = Arrays.asList(category);
    getCategoryPage = new PageImpl<>(categories, CategoryServiceTest.DEFAULT_PAGEABLE, categories.size());
    Mockito.when(this.categoryRepository
        .findCategoryByStoreIdAndNameAndStateAndDocumentFilterType(STORE_ID, CATEGORY1_NAME, ACTIVE_STATE,
            DOCUMENT_REQUIRED, DEFAULT_PAGEABLE))
        .thenReturn(getCategoryPage);

    Page<Category> categoryPage = this.categoryServiceBean
        .findByName(CategoryServiceTest.STORE_ID, CATEGORY1_NAME, CategoryServiceTest.DEFAULT_PAGEABLE,
            CategoryServiceTest.ACTIVE_STATE, DOCUMENT_REQUIRED);

    verify(this.categoryRepository, times(1))
        .findCategoryByStoreIdAndNameAndStateAndDocumentFilterType(STORE_ID, CATEGORY1_NAME, ACTIVE_STATE,
            DOCUMENT_REQUIRED, DEFAULT_PAGEABLE);
    assertTrue(DOCUMENTS.contains(categoryPage.getContent().get(0).getDocumentType()));
  }

  @Test
  public void testFindByName_ForStateIsActiveAndDocumentNull() {
    category = this.getDefaultCategory(1);
    category.setDocumentType(null);
    categories = Arrays.asList(category);
    getCategoryPage = new PageImpl<>(categories, DEFAULT_PAGEABLE, categories.size());
    Mockito.when(this.categoryRepository
        .findCategoryByStoreIdAndNameAndStateAndDocumentFilterType(STORE_ID, CATEGORY1_NAME, ACTIVE_STATE,
            DOCUMENT_NOT_REQUIRED, DEFAULT_PAGEABLE)).thenReturn(getCategoryPage);

    Page<Category> categoryPage =
        this.categoryServiceBean.findByName(STORE_ID, CATEGORY1_NAME, DEFAULT_PAGEABLE, ACTIVE_STATE, DOCUMENT_NOT_REQUIRED);

    verify(this.categoryRepository, times(1))
        .findCategoryByStoreIdAndNameAndStateAndDocumentFilterType(STORE_ID, CATEGORY1_NAME, ACTIVE_STATE,
            DOCUMENT_NOT_REQUIRED, DEFAULT_PAGEABLE);
    assertTrue(StringUtils.isEmpty(categoryPage.getContent().get(0).getDocumentType()));
  }

  @Test
  public void testFindByName_ForStateIsInActiveAndDocumentNotNull() {
    category = this.getDefaultCategory(1);
    category.setDocumentType(DOCUMENTS);
    categories = Arrays.asList(category);
    getCategoryPage = new PageImpl<>(categories, DEFAULT_PAGEABLE, categories.size());
    Mockito.when(this.categoryRepository
        .findCategoryByStoreIdAndNameAndStateAndDocumentFilterType(STORE_ID, CATEGORY1_NAME, INACTIVE_STATE,
            DOCUMENT_REQUIRED, DEFAULT_PAGEABLE)).thenReturn(getCategoryPage);

    Page<Category> categoryPage =
        this.categoryServiceBean.findByName(STORE_ID, CATEGORY1_NAME, DEFAULT_PAGEABLE, INACTIVE_STATE, DOCUMENT_REQUIRED);

    verify(this.categoryRepository, times(1))
        .findCategoryByStoreIdAndNameAndStateAndDocumentFilterType(STORE_ID, CATEGORY1_NAME, INACTIVE_STATE,
            DOCUMENT_REQUIRED, DEFAULT_PAGEABLE);
    assertTrue(DOCUMENTS.contains(categoryPage.getContent().get(0).getDocumentType()));
  }

  @Test
  public void testFindByName_ForStateIsInActiveAndDocumentNull() {
    category = this.getDefaultCategory(1);
    category.setDocumentType(null);
    categories = Arrays.asList(category);
    getCategoryPage = new PageImpl<>(categories, DEFAULT_PAGEABLE, categories.size());
    Mockito.when(this.categoryRepository
        .findCategoryByStoreIdAndNameAndStateAndDocumentFilterType(STORE_ID, CATEGORY1_NAME, INACTIVE_STATE,
            DOCUMENT_NOT_REQUIRED, DEFAULT_PAGEABLE)).thenReturn(getCategoryPage);

    Page<Category> categoryPage = this.categoryServiceBean
        .findByName(STORE_ID, CATEGORY1_NAME, DEFAULT_PAGEABLE, INACTIVE_STATE, DOCUMENT_NOT_REQUIRED);

    verify(this.categoryRepository, times(1))
        .findCategoryByStoreIdAndNameAndStateAndDocumentFilterType(STORE_ID, CATEGORY1_NAME, INACTIVE_STATE,
            DOCUMENT_NOT_REQUIRED, DEFAULT_PAGEABLE);
    assertTrue(StringUtils.isEmpty(categoryPage.getContent().get(0).getDocumentType()));
  }

  @Test
  public void testFindByStoreId() throws Exception {
    List<Category> categories = this.getDefaultCategories();
    Page<Category> categoryPage =
        new PageImpl<Category>(categories, CategoryServiceTest.DEFAULT_PAGEABLE, categories.size());
    Mockito
        .when(this.categoryRepository.findByStoreIdAndMarkForDeleteFalse(
            CategoryServiceTest.STORE_ID, CategoryServiceTest.DEFAULT_PAGEABLE))
        .thenReturn(categoryPage);
    assertTrue(this.categoryServiceBean
        .findByStoreId(CategoryServiceTest.STORE_ID, CategoryServiceTest.DEFAULT_PAGEABLE)
        .getTotalElements() == categoryPage.getTotalElements());
    verify(this.categoryRepository, times(1)).findByStoreIdAndMarkForDeleteFalse(
        CategoryServiceTest.STORE_ID, CategoryServiceTest.DEFAULT_PAGEABLE);
  }

  @Test
  public void testFindByStoreIdAndCategoryCode() throws Exception {
    Category category =
        new Category(CategoryServiceTest.STORE_ID, CategoryServiceTest.CATEGORY1_NAME, 1);
    category.setCategoryCode(CategoryServiceTest.CATEGORY1_CODE);
    Mockito.when(this.categoryRepository.findByStoreIdAndCategoryCodeAndMarkForDeleteFalse(
        CategoryServiceTest.STORE_ID, CategoryServiceTest.CATEGORY1_CODE)).thenReturn(category);
    this.categoryServiceBean.findByStoreIdAndCategoryCode(CategoryServiceTest.STORE_ID,
        CategoryServiceTest.CATEGORY1_CODE);
    verify(this.categoryRepository).findByStoreIdAndCategoryCodeAndMarkForDeleteFalse(
        CategoryServiceTest.STORE_ID, CategoryServiceTest.CATEGORY1_CODE);
  }

  @Test
  public void testFindBasicInfoByStoreIdAndCategoryCode() throws Exception {
    Category category =
        new Category(CategoryServiceTest.STORE_ID, CategoryServiceTest.CATEGORY1_NAME, 1);
    category.setCategoryCode(CategoryServiceTest.CATEGORY1_CODE);
    Mockito.when(this.categoryRepository.findByStoreIdAndCategoryCodeAndMarkForDeleteFalse(
        CategoryServiceTest.STORE_ID, CategoryServiceTest.CATEGORY1_CODE)).thenReturn(category);
    this.categoryServiceBean.findBasicInfoByStoreIdAndCategoryCode(CategoryServiceTest.STORE_ID,
        CategoryServiceTest.CATEGORY1_CODE);
    verify(this.categoryRepository).findByStoreIdAndCategoryCodeAndMarkForDeleteFalse(
        CategoryServiceTest.STORE_ID, CategoryServiceTest.CATEGORY1_CODE);
  }

  @Test
  public void testFindBasicInfoByStoreIdAndCategoryCodeFail() throws Exception {
    Category category = new Category(CategoryServiceTest.STORE_ID, CategoryServiceTest.CATEGORY1_NAME, 1);
    category.setCategoryCode(CategoryServiceTest.CATEGORY1_CODE);
    Mockito.when(this.categoryRepository.findByStoreIdAndCategoryCodeAndMarkForDeleteFalse(CategoryServiceTest.STORE_ID,
        CategoryServiceTest.CATEGORY1_CODE)).thenReturn(null);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.categoryServiceBean.findBasicInfoByStoreIdAndCategoryCode(CategoryServiceTest.STORE_ID,
          CategoryServiceTest.CATEGORY1_CODE));
    } finally {
      verify(this.categoryRepository).findByStoreIdAndCategoryCodeAndMarkForDeleteFalse(CategoryServiceTest.STORE_ID,
          CategoryServiceTest.CATEGORY1_CODE);
    }
  }

  @Test
  public void testFindByStoreIdAndCategoryId() {
    Category category = new Category(CategoryServiceTest.STORE_ID, CategoryServiceTest.CATEGORY1_NAME, 1);
    category.setId(CategoryServiceTest.CATEGORY_ID);
    category.setCategoryCode(CategoryServiceTest.CATEGORY1_CODE);
    category.setUmkm(true);
    Mockito.when(this.categoryRepository
        .findByStoreIdAndIdAndMarkForDeleteFalse(CategoryServiceTest.STORE_ID, CategoryServiceTest.CATEGORY_ID))
        .thenReturn(category);
    Category response = this.categoryServiceBean
        .findByStoreIdAndIdInitCategoryAttribute(CategoryServiceTest.STORE_ID, CategoryServiceTest.CATEGORY_ID);
    verify(this.categoryRepository)
        .findByStoreIdAndIdAndMarkForDeleteFalse(CategoryServiceTest.STORE_ID, CategoryServiceTest.CATEGORY_ID);
    assertTrue(response.isUmkm());
  }

  @Test
  public void testFindByStoreIdAndCategoryIdWithException() throws Exception {
    Mockito.when(applicationContext.getBean(CategoryService.class)).thenReturn(categoryServiceBean);
    Mockito.when(categoryRepository
        .findByStoreIdAndIdAndMarkForDeleteFalse(CategoryServiceTest.STORE_ID, CategoryServiceTest.CATEGORY_ID))
        .thenReturn(null);
    try {
      this.categoryServiceBean
          .findByStoreIdAndIdInitCategoryAttribute(CategoryServiceTest.STORE_ID, CategoryServiceTest.CATEGORY_ID);
    } catch (ApplicationRuntimeException e) {
      verify(categoryRepository).findByStoreIdAndIdAndMarkForDeleteFalse(CategoryServiceTest.STORE_ID,
          CategoryServiceTest.CATEGORY_ID);
    }
  }

  @Test
  public void testFindShippingInfoByStoreIdCategoryCode() {
    Mockito.when(this.categoryShippingService
        .findByCategoryCode(CategoryServiceTest.STORE_ID, CategoryServiceTest.CATEGORY1_CODE))
        .thenReturn(new ArrayList<>());
    List<CategoryShipping> categoryShippingList = this.categoryServiceBean
        .findShippingInfoByStoreIdCategoryCode(CategoryServiceTest.STORE_ID, CategoryServiceTest.CATEGORY1_CODE);
    verify(this.categoryShippingService)
        .findByCategoryCode(CategoryServiceTest.STORE_ID, CategoryServiceTest.CATEGORY1_CODE);
    assertNotNull(categoryShippingList);
  }

  @Test
  public void testFindByStoreIdAndCategoryCodes() throws Exception {
    List<Category> categories = getDefaultCategoriesWithCategoryCode();
    List<String> categoryCodes = new ArrayList<String>();
    for (Category category : categories)
      categoryCodes.add(category.getCategoryCode());
    Page<Category> result =
        new PageImpl<Category>(categories, this.defaultPageable, categories.size());
    Mockito.when(this.categoryRepository.findByStoreIdAndCategoryCodesAndMarkForDeleteFalse(
        CategoryServiceTest.STORE_ID, categoryCodes, this.defaultPageable)).thenReturn(result);
    this.categoryServiceBean.findByStoreIdAndCategoryCodes(CategoryServiceTest.STORE_ID,
        categoryCodes, null, this.defaultPageable);
    verify(this.categoryRepository).findByStoreIdAndCategoryCodesAndMarkForDeleteFalse(
        CategoryServiceTest.STORE_ID, categoryCodes, this.defaultPageable);
  }

  @Test
  public void testfindByStoreIdAndCategoryCodes() throws Exception {
    List<Category> categories = getDefaultCategoriesWithCategoryCode();
    List<String> categoryCodes = new ArrayList<String>();
    for (Category category : categories)
      categoryCodes.add(category.getCategoryCode());
    List<Category> result = new ArrayList<>(categories);
    Mockito.when(this.categoryRepository
        .findByStoreIdAndCategoryCodeInAndMarkForDeleteFalse(CategoryServiceTest.STORE_ID, categoryCodes))
        .thenReturn(result);
    this.categoryServiceBean.findByStoreIdAndCategoryCodes(CategoryServiceTest.STORE_ID, categoryCodes);
    verify(this.categoryRepository)
        .findByStoreIdAndCategoryCodeInAndMarkForDeleteFalse(CategoryServiceTest.STORE_ID, categoryCodes);
  }

  @Test
  public void testfindByStoreIdAndCategoryCodes_emptyCodes() throws Exception {
    List<String> categoryCodes = new ArrayList<String>();
    List<Category> result = null;
    try {
      result = this.categoryServiceBean.findByStoreIdAndCategoryCodes(CategoryServiceTest.STORE_ID,
        categoryCodes);
    } catch (ApplicationRuntimeException e) {
    }
    Assertions.assertNull(result);
  }

  @Test
  public void testFindByStoreIdAndCategoryCodesWithNullResult() throws Exception {
    List<String> categoryCodes = new ArrayList<String>();
    categoryCodes.add("NOT_FOUND");
    Page<Category> result = new PageImpl<Category>(new ArrayList<>());
    try {
      Mockito
          .when(this.categoryRepository.findByStoreIdAndCategoryCodesAndMarkForDeleteFalse(
              CategoryServiceTest.STORE_ID, categoryCodes, this.defaultPageable))
          .thenReturn(result);
      this.categoryServiceBean.findByStoreIdAndCategoryCodes(CategoryServiceTest.STORE_ID,
          categoryCodes, null, this.defaultPageable);
    } catch (Exception e) {
      verify(this.categoryRepository).findByStoreIdAndCategoryCodesAndMarkForDeleteFalse(
          CategoryServiceTest.STORE_ID, categoryCodes, this.defaultPageable);

    }
  }

  @Test
  public void testFindByStoreIdAndCategoryCodesWithEmptyResult() throws Exception {
    List<String> categoryCodes = new ArrayList<String>();
    categoryCodes.add("NOT_FOUND");
    Page<Category> result = new PageImpl<Category>(new ArrayList<Category>());
    try {
      Mockito
          .when(this.categoryRepository.findByStoreIdAndCategoryCodesAndMarkForDeleteFalse(
              CategoryServiceTest.STORE_ID, categoryCodes, this.defaultPageable))
          .thenReturn(result);
      this.categoryServiceBean.findByStoreIdAndCategoryCodes(CategoryServiceTest.STORE_ID,
          categoryCodes, null, this.defaultPageable);
    } catch (Exception e) {
      verify(this.categoryRepository).findByStoreIdAndCategoryCodesAndMarkForDeleteFalse(
          CategoryServiceTest.STORE_ID, categoryCodes, this.defaultPageable);
    }
  }

  @Test
  public void testFindByStoreIdAndCategoryCodesAndActivated() throws Exception {
    Boolean activated = true;
    List<Category> categories = getDefaultCategoriesWithCategoryCode();
    List<String> categoryCodes = new ArrayList<String>();
    for (Category category : categories)
      categoryCodes.add(category.getCategoryCode());
    Page<Category> result =
        new PageImpl<Category>(categories, this.defaultPageable, categories.size());
    Mockito
        .when(
            this.categoryRepository.findByStoreIdAndCategoryCodesAndActivatedAndMarkForDeleteFalse(
                CategoryServiceTest.STORE_ID, categoryCodes, activated, this.defaultPageable))
        .thenReturn(result);
    this.categoryServiceBean.findByStoreIdAndCategoryCodes(CategoryServiceTest.STORE_ID,
        categoryCodes, activated, this.defaultPageable);
    verify(this.categoryRepository)
        .findByStoreIdAndCategoryCodesAndActivatedAndMarkForDeleteFalse(
            CategoryServiceTest.STORE_ID, categoryCodes, activated, this.defaultPageable);
  }

  @Test
  public void testFindByStoreIdAndCategoryCodesAndActivatedWithNullResult() throws Exception {
    Boolean activated = true;
    List<String> categoryCodes = new ArrayList<String>();
    categoryCodes.add("NOT_FOUND");
    Page<Category> result = new PageImpl<Category>(new ArrayList<>());
    try {
      Mockito
          .when(this.categoryRepository
              .findByStoreIdAndCategoryCodesAndActivatedAndMarkForDeleteFalse(
                  CategoryServiceTest.STORE_ID, categoryCodes, activated, this.defaultPageable))
          .thenReturn(result);
      this.categoryServiceBean.findByStoreIdAndCategoryCodes(CategoryServiceTest.STORE_ID,
          categoryCodes, activated, this.defaultPageable);
    } catch (Exception e) {
      verify(this.categoryRepository)
          .findByStoreIdAndCategoryCodesAndActivatedAndMarkForDeleteFalse(
              CategoryServiceTest.STORE_ID, categoryCodes, activated, this.defaultPageable);
    }
  }

  @Test
  public void testFindByStoreIdAndCategoryCodesAndActivatedWithEmptyResult() throws Exception {
    Boolean activated = true;
    List<String> categoryCodes = new ArrayList<String>();
    categoryCodes.add("NOT_FOUND");
    Page<Category> result = new PageImpl<Category>(new ArrayList<Category>());
    try {
      Mockito
          .when(this.categoryRepository
              .findByStoreIdAndCategoryCodesAndActivatedAndMarkForDeleteFalse(
                  CategoryServiceTest.STORE_ID, categoryCodes, activated, this.defaultPageable))
          .thenReturn(result);
      this.categoryServiceBean.findByStoreIdAndCategoryCodes(CategoryServiceTest.STORE_ID,
          categoryCodes, activated, this.defaultPageable);
    } catch (Exception e) {
      verify(this.categoryRepository)
          .findByStoreIdAndCategoryCodesAndActivatedAndMarkForDeleteFalse(
              CategoryServiceTest.STORE_ID, categoryCodes, activated, this.defaultPageable);
    }
  }

  @Test
  public void testFindByStoreIdAndId() {
    String id = UUID.randomUUID().toString();
    Category category = this.getDefaultCategory(1);
    category.setId(id);
    Mockito
        .when(this.categoryRepository
            .findByStoreIdAndIdAndMarkForDeleteFalse(CategoryServiceTest.STORE_ID, id))
        .thenReturn(category);
    this.categoryServiceBean.findByStoreIdAndId(CategoryServiceTest.STORE_ID, id);
    verify(this.categoryRepository)
        .findByStoreIdAndIdAndMarkForDeleteFalse(CategoryServiceTest.STORE_ID, id);
  }

  @Test
  public void testFindByStoreIdAndIdInitCategoryAttribute() {
    String id = UUID.randomUUID().toString();
    Category category = this.getDefaultCategory(1);
    category.setId(id);
    Mockito.when(categoryRepository.findByStoreIdAndIdAndMarkForDeleteFalse(STORE_ID, category.getId()))
        .thenReturn(category);

    this.categoryServiceBean.findByStoreIdAndIdInitCategoryAttribute(CategoryServiceTest.STORE_ID, id);
    verify(categoryRepository).findByStoreIdAndIdAndMarkForDeleteFalse(STORE_ID, id);
  }

  @Test
  public void findCountByParentCategoryAndHalalCategoryTest() {
    Mockito.when(categoryRepository.countByStoreIdAndParentCategoryAndHalalCategoryAndMarkForDeleteFalse(STORE_ID, category, true)).thenReturn(2l);
    this.categoryServiceBean.findCountByParentCategoryAndHalalCategory(STORE_ID, category, true);
    verify(categoryRepository).countByStoreIdAndParentCategoryAndHalalCategoryAndMarkForDeleteFalse(STORE_ID, category, true);
  }

  @Test
  public void testFindByStoreIdAndIdInitCategoryAttributeForMFD() {
    String id = UUID.randomUUID().toString();
    Category category = this.getDefaultCategory(1);
    category.setId(id);
    categoryAttribute.setMarkForDelete(true);
    categoryAttribute.setId(id);
    categoryAttribute.setAttribute(new Attribute());
    categoryAttribute.getAttribute().setId(id);
    categoryAttribute.getAttribute().setName("Warna");
    categoryAttribute.getAttribute().setAttributeType(AttributeType.DEFINING_ATTRIBUTE);
    categoryAttributes.add(categoryAttribute);
    category.setCategoryAttributes(categoryAttributes);
    Mockito
        .when(this.categoryRepository
            .findByStoreIdAndIdAndMarkForDeleteFalse(CategoryServiceTest.STORE_ID, id))
        .thenReturn(category);
    this.categoryServiceBean.findByStoreIdAndIdInitCategoryAttribute(CategoryServiceTest.STORE_ID, id);
    verify(this.categoryRepository).findByStoreIdAndIdAndMarkForDeleteFalse(CategoryServiceTest.STORE_ID, id);
    assertEquals(0, category.getCategoryAttributes().size());
  }

  @Test
  public void testFindByStoreIdAndIdInitCategoryAttributeWithEmptyCategoryId() {
    String id = UUID.randomUUID().toString();
    Mockito.when(applicationContext.getBean(CategoryService.class)).thenReturn(categoryServiceBean);
    Mockito.when(categoryRepository.findByStoreIdAndIdAndMarkForDeleteFalse(CategoryServiceTest.STORE_ID, id))
        .thenReturn(null);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.categoryServiceBean.findByStoreIdAndIdInitCategoryAttribute(CategoryServiceTest.STORE_ID,
          id));
    } finally {
      verify(this.categoryRepository).findByStoreIdAndIdAndMarkForDeleteFalse(CategoryServiceTest.STORE_ID, id);
    }
  }

  @Test
  public void testFindCategoryHierarchyByCategoryCode() throws Exception {
    Category c1 = new Category(STORE_ID, "c1", 0);
    Category c2 = new Category(STORE_ID, "c2", 1);
    c2.setParentCategory(c1);
    Category c3 = new Category(STORE_ID, "c3", 2);
    c3.setParentCategory(c2);
    Category c4 = new Category(STORE_ID, "c4", 3);
    c4.setParentCategory(c3);
    c4.setCategoryCode("category-4");
    Mockito.when(this.categoryRepository.findByStoreIdAndCategoryCodeAndMarkForDeleteFalse(STORE_ID,
        "category-4")).thenReturn(c4);
    this.categoryServiceBean.findCategoryHierarchyByCategoryCode(STORE_ID, "category-4");
    verify(applicationContext).getBean(CategoryService.class);
  }


  @Test
  public void caffeineFindCategoryHierarchyByCategoryCodeTest() throws Exception {
    Category c1 = new Category(STORE_ID, "c1", 0);
    c1.setId(CATEGORY_ID);
    Category c2 = new Category(STORE_ID, "c2", 1);
    c2.setParentCategory(c1);
    c2.setId(CATEGORY_ID1);
    Category c3 = new Category(STORE_ID, "c3", 2);
    c3.setParentCategory(c2);
    c3.setId(CATEGORY_ID2);
    Category c4 = new Category(STORE_ID, "c4", 3);
    c4.setParentCategory(c3);
    c4.setId(CATEGORY_ID3);
    ReflectionTestUtils.setField(categoryServiceBean, "caffeineCacheEnabled", true);
    Mockito.when(categoryService.getCategoryByStoreIdAndIdCached(STORE_ID, CATEGORY_ID3))
        .thenReturn(c4);
    Mockito.when(applicationContext.getBean(CategoryService.class)).thenReturn(categoryService);
    List<Category> categories = this.categoryServiceBean.findCategoryHierarchyByCategoryCode(STORE_ID, CATEGORY_ID3);
    verify(applicationContext).getBean(CategoryService.class);
  }



  @Test
  public void findCategoryHierarchyByCategoryIdTest() throws Exception {
    Category c1 = new Category(STORE_ID, "c1", 0);
    c1.setId(CATEGORY_ID);
    Category c2 = new Category(STORE_ID, "c2", 1);
    c2.setParentCategory(c1);
    c2.setId(CATEGORY_ID1);
    Category c3 = new Category(STORE_ID, "c3", 2);
    c3.setParentCategory(c2);
    c3.setId(CATEGORY_ID2);
    Category c4 = new Category(STORE_ID, "c4", 3);
    c4.setParentCategory(c3);
    c4.setId(CATEGORY_ID3);
    Mockito.when(categoryService.getCategoryByStoreIdAndIdCached(STORE_ID, CATEGORY_ID3))
        .thenReturn(c4);
    Mockito.when(applicationContext.getBean(CategoryService.class)).thenReturn(categoryService);
    List<Category> categories = this.categoryServiceBean.findCategoryHierarchyByCategoryId(STORE_ID, CATEGORY_ID3);
    verify(applicationContext).getBean(CategoryService.class);
    verify(categoryService).getCategoryByStoreIdAndIdCached(STORE_ID, CATEGORY_ID3);
    assertEquals(4, categories.size());
  }

  @Test
  public void testFindChildForParent() {
    Category parentCategory =
        new Category(CategoryServiceTest.STORE_ID, CategoryServiceTest.CATEGORY1_NAME, 1);
    Category childCategory1 =
        new Category(CategoryServiceTest.STORE_ID, CategoryServiceTest.CATEGORY1_CHILD1_NAME, 1);
    Category childCategory2 =
        new Category(CategoryServiceTest.STORE_ID, CategoryServiceTest.CATEGORY1_CHILD2_NAME, 1);
    Category childCategory3 =
        new Category(CategoryServiceTest.STORE_ID, CategoryServiceTest.CATEGORY1_CHILD3_NAME, 1);
    childCategory1.setParentCategory(parentCategory);
    childCategory2.setParentCategory(parentCategory);
    childCategory3.setParentCategory(parentCategory);
    List<Category> categories = new ArrayList<Category>();
    categories.add(childCategory1);
    categories.add(childCategory2);
    categories.add(childCategory3);

    Page<Category> categoryPage =
        new PageImpl<Category>(categories, CategoryServiceTest.DEFAULT_PAGEABLE, categories.size());

    Mockito.when(this.categoryRepository.findByStoreIdAndParentCategoryAndMarkForDeleteFalse(
        CategoryServiceTest.STORE_ID, parentCategory)).thenReturn(categories);
    Mockito
        .when(this.categoryRepository.findByStoreIdAndParentCategoryAndMarkForDeleteFalse(
            CategoryServiceTest.STORE_ID, parentCategory, CategoryServiceTest.DEFAULT_PAGEABLE))
        .thenReturn(categoryPage);

    assertTrue(
        this.categoryServiceBean.findChildForParent(CategoryServiceTest.STORE_ID, parentCategory)
            .size() == this.categoryServiceBean.findChildForParent(CategoryServiceTest.STORE_ID,
                parentCategory, CategoryServiceTest.DEFAULT_PAGEABLE).getTotalElements());

    verify(this.categoryRepository, times(1))
        .findByStoreIdAndParentCategoryAndMarkForDeleteFalse(CategoryServiceTest.STORE_ID,
            parentCategory);
    verify(this.categoryRepository, times(1))
        .findByStoreIdAndParentCategoryAndMarkForDeleteFalse(CategoryServiceTest.STORE_ID,
            parentCategory, CategoryServiceTest.DEFAULT_PAGEABLE);
  }

  @Test
  public void findChildForParentByCatalogId() throws Exception {
    List<Category> categories = this.getDefaultCategoriesWithCategoryCode();
    categories.get(0).setGenericTemplateEligible(true);
    categories.get(1).setGenericTemplateEligible(false);
    categoryPage = new PageImpl<>(categories, CategoryServiceTest.DEFAULT_PAGEABLE, categories.size());
    Mockito.when(
            this.categoryRepository.findByStoreIdAndParentCategoryIdAndCatalogIdAndDocumentFilterTypeAndB2bExclusiveAndHalalCategoryAndMarkForDeleteFalse(
                STORE_ID, CATEGORY_ID, CATEGORY1_CHILD1_NAME, ALL, false, false, this.defaultPageable))
        .thenReturn(this.categoryPage);
    Page<Category> response =
        this.categoryServiceBean.findChildForParentByCatalogId(STORE_ID, CATEGORY_ID, CATEGORY1_CHILD1_NAME, ALL, false,
            false, this.defaultPageable);
    verify(
        this.categoryRepository).findByStoreIdAndParentCategoryIdAndCatalogIdAndDocumentFilterTypeAndB2bExclusiveAndHalalCategoryAndMarkForDeleteFalse(
        STORE_ID, CATEGORY_ID, CATEGORY1_CHILD1_NAME, ALL, false, false, this.defaultPageable);
    assertTrue(response.getContent().get(0).isGenericTemplateEligible());
    assertFalse(response.getContent().get(1).isGenericTemplateEligible());
  }

  @Test
  public void findChildForParentByCatalogIdWithDocumentFilterType() throws Exception {
    List<Category> categories = this.getDefaultCategoriesWithCategoryCode();
    categories.get(0).setGenericTemplateEligible(true);
    categories.get(1).setGenericTemplateEligible(false);
    categoryPage = new PageImpl<>(categories, CategoryServiceTest.DEFAULT_PAGEABLE, categories.size());
    Mockito.when(
            this.categoryRepository.findByStoreIdAndParentCategoryIdAndCatalogIdAndDocumentFilterTypeAndB2bExclusiveAndHalalCategoryAndMarkForDeleteFalse(
                STORE_ID, CATEGORY_ID, CATEGORY1_CHILD1_NAME, DOCUMENT_REQUIRED, false, false, this.defaultPageable))
        .thenReturn(this.categoryPage);
    Page<Category> response =
        this.categoryServiceBean.findChildForParentByCatalogId(STORE_ID, CATEGORY_ID, CATEGORY1_CHILD1_NAME,
            DOCUMENT_REQUIRED, false, false, this.defaultPageable);
    verify(
        this.categoryRepository).findByStoreIdAndParentCategoryIdAndCatalogIdAndDocumentFilterTypeAndB2bExclusiveAndHalalCategoryAndMarkForDeleteFalse(
        STORE_ID, CATEGORY_ID, CATEGORY1_CHILD1_NAME, DOCUMENT_REQUIRED, false, false, this.defaultPageable);
    assertTrue(response.getContent().get(0).isGenericTemplateEligible());
    assertFalse(response.getContent().get(1).isGenericTemplateEligible());
  }

  @Test
  public void findChildForParentByCatalogIdWithNoDocumentFilterType() throws Exception {
    List<Category> categories = this.getDefaultCategoriesWithCategoryCode();
    categories.get(0).setGenericTemplateEligible(true);
    categories.get(1).setGenericTemplateEligible(false);
    categoryPage = new PageImpl<>(categories, CategoryServiceTest.DEFAULT_PAGEABLE, categories.size());
    Mockito.when(
            this.categoryRepository.findByStoreIdAndParentCategoryIdAndCatalogIdAndDocumentFilterTypeAndB2bExclusiveAndHalalCategoryAndMarkForDeleteFalse(
                STORE_ID, CATEGORY_ID, CATEGORY1_CHILD1_NAME, DOCUMENT_NOT_REQUIRED, false, false, this.defaultPageable))
        .thenReturn(this.categoryPage);
    Page<Category> response =
        this.categoryServiceBean.findChildForParentByCatalogId(STORE_ID, CATEGORY_ID, CATEGORY1_CHILD1_NAME,
            DOCUMENT_NOT_REQUIRED, false, false, this.defaultPageable);
    verify(
        this.categoryRepository).findByStoreIdAndParentCategoryIdAndCatalogIdAndDocumentFilterTypeAndB2bExclusiveAndHalalCategoryAndMarkForDeleteFalse(
        STORE_ID, CATEGORY_ID, CATEGORY1_CHILD1_NAME, DOCUMENT_NOT_REQUIRED, false, false, this.defaultPageable);
    assertTrue(response.getContent().get(0).isGenericTemplateEligible());
    assertFalse(response.getContent().get(1).isGenericTemplateEligible());
  }

  @Test
  public void testFindChildForParentWithCatalogType() {
    Catalog catalog = new Catalog("catalog-name", "catalog-code", CatalogType.MASTER_CATALOG,
        CategoryServiceTest.STORE_ID);
    Category parentCategory =
        new Category(CategoryServiceTest.STORE_ID, CategoryServiceTest.CATEGORY1_NAME, 1);
    parentCategory.setCatalog(catalog);
    Category childCategory1 =
        new Category(CategoryServiceTest.STORE_ID, CategoryServiceTest.CATEGORY1_CHILD1_NAME, 1);
    Category childCategory2 =
        new Category(CategoryServiceTest.STORE_ID, CategoryServiceTest.CATEGORY1_CHILD2_NAME, 1);
    Category childCategory3 =
        new Category(CategoryServiceTest.STORE_ID, CategoryServiceTest.CATEGORY1_CHILD3_NAME, 1);
    childCategory1.setParentCategory(parentCategory);
    childCategory1.setCatalog(catalog);
    childCategory2.setParentCategory(parentCategory);
    childCategory2.setCatalog(catalog);
    childCategory3.setParentCategory(parentCategory);
    childCategory3.setCatalog(catalog);
    childCategory1.setActivated(true);
    childCategory2.setActivated(true);
    childCategory3.setActivated(false);
    List<Category> categories = new ArrayList<Category>();
    categories.add(childCategory1);
    categories.add(childCategory2);
    categories.add(childCategory3);

    Page<Category> categoryPage =
        new PageImpl<Category>(categories, CategoryServiceTest.DEFAULT_PAGEABLE, categories.size());

    Mockito
        .when(this.categoryRepository
            .findByStoreIdAndParentCategoryAndCatalog_CatalogTypeAndMarkForDeleteFalse(
                CategoryServiceTest.STORE_ID, parentCategory, CatalogType.MASTER_CATALOG))
        .thenReturn(categories);
    Mockito.when(this.categoryRepository
        .findByStoreIdAndParentCategoryAndCatalog_CatalogTypeAndMarkForDeleteFalse(
            CategoryServiceTest.STORE_ID, parentCategory, CatalogType.MASTER_CATALOG,
            CategoryServiceTest.DEFAULT_PAGEABLE))
        .thenReturn(categoryPage);

    assertTrue(this.categoryServiceBean
        .findChildForParentWithCatalogType(CategoryServiceTest.STORE_ID, parentCategory,
            CatalogType.MASTER_CATALOG, null)
        .size() == this.categoryServiceBean
            .findChildForParentWithCatalogType(CategoryServiceTest.STORE_ID, parentCategory,
                CatalogType.MASTER_CATALOG, null, CategoryServiceTest.DEFAULT_PAGEABLE)
            .getTotalElements());

    verify(this.categoryRepository, times(1))
        .findByStoreIdAndParentCategoryAndCatalog_CatalogTypeAndMarkForDeleteFalse(
            CategoryServiceTest.STORE_ID, parentCategory, CatalogType.MASTER_CATALOG);
    verify(this.categoryRepository, times(1))
        .findByStoreIdAndParentCategoryAndCatalog_CatalogTypeAndMarkForDeleteFalse(
            CategoryServiceTest.STORE_ID, parentCategory, CatalogType.MASTER_CATALOG,
            CategoryServiceTest.DEFAULT_PAGEABLE);
  }

  @Test
  public void testFindChildForParentWithCatalogTypeAndActivated() {
    Boolean activated = true;
    Catalog catalog = new Catalog("catalog-name", "catalog-code", CatalogType.MASTER_CATALOG,
        CategoryServiceTest.STORE_ID);
    Category parentCategory =
        new Category(CategoryServiceTest.STORE_ID, CategoryServiceTest.CATEGORY1_NAME, 1);
    parentCategory.setCatalog(catalog);
    Category childCategory1 =
        new Category(CategoryServiceTest.STORE_ID, CategoryServiceTest.CATEGORY1_CHILD1_NAME, 1);
    Category childCategory2 =
        new Category(CategoryServiceTest.STORE_ID, CategoryServiceTest.CATEGORY1_CHILD2_NAME, 1);
    Category childCategory3 =
        new Category(CategoryServiceTest.STORE_ID, CategoryServiceTest.CATEGORY1_CHILD3_NAME, 1);
    childCategory1.setParentCategory(parentCategory);
    childCategory1.setCatalog(catalog);
    childCategory2.setParentCategory(parentCategory);
    childCategory2.setCatalog(catalog);
    childCategory3.setParentCategory(parentCategory);
    childCategory3.setCatalog(catalog);
    childCategory1.setActivated(true);
    childCategory2.setActivated(true);
    childCategory3.setActivated(false);
    List<Category> categories = new ArrayList<Category>();
    categories.add(childCategory1);
    categories.add(childCategory2);
    categories.add(childCategory3);

    Page<Category> categoryPage =
        new PageImpl<Category>(categories, CategoryServiceTest.DEFAULT_PAGEABLE, categories.size());

    Mockito.when(this.categoryRepository
        .findByStoreIdAndParentCategoryAndCatalog_CatalogTypeAndActivatedAndMarkForDeleteFalse(
            CategoryServiceTest.STORE_ID, parentCategory, CatalogType.MASTER_CATALOG, activated))
        .thenReturn(categories);
    Mockito.when(this.categoryRepository
        .findByStoreIdAndParentCategoryAndCatalog_CatalogTypeAndActivatedAndMarkForDeleteFalse(
            CategoryServiceTest.STORE_ID, parentCategory, CatalogType.MASTER_CATALOG, activated,
            CategoryServiceTest.DEFAULT_PAGEABLE))
        .thenReturn(categoryPage);

    assertTrue(this.categoryServiceBean
        .findChildForParentWithCatalogType(CategoryServiceTest.STORE_ID, parentCategory,
            CatalogType.MASTER_CATALOG, activated)
        .size() == this.categoryServiceBean
            .findChildForParentWithCatalogType(CategoryServiceTest.STORE_ID, parentCategory,
                CatalogType.MASTER_CATALOG, activated, CategoryServiceTest.DEFAULT_PAGEABLE)
            .getTotalElements());

    verify(this.categoryRepository, times(1))
        .findByStoreIdAndParentCategoryAndCatalog_CatalogTypeAndActivatedAndMarkForDeleteFalse(
            CategoryServiceTest.STORE_ID, parentCategory, CatalogType.MASTER_CATALOG, activated);
    verify(this.categoryRepository, times(1))
        .findByStoreIdAndParentCategoryAndCatalog_CatalogTypeAndActivatedAndMarkForDeleteFalse(
            CategoryServiceTest.STORE_ID, parentCategory, CatalogType.MASTER_CATALOG, activated,
            CategoryServiceTest.DEFAULT_PAGEABLE);
  }

  @Test
  public void testMarkForDeleteCategory() throws Exception {
    Category savedCategory = new Category();
    ProductCategory productCategory =
        new ProductCategory(new Product(), this.category, CategoryServiceTest.STORE_ID);
    ProductCategory savedProductCategory =
        new ProductCategory(new Product(), savedCategory, CategoryServiceTest.STORE_ID);
    CategoryAttribute categoryAttribute = new CategoryAttribute(savedCategory, new Attribute(),
        Integer.valueOf(0), true, true, CategoryServiceTest.STORE_ID);
    CategoryAttribute savedCategoryAttribute = new CategoryAttribute(savedCategory, new Attribute(),
        Integer.valueOf(0), true, true, CategoryServiceTest.STORE_ID);

    String uuid = GdnUUIDHelper.generateUUID();
    String uuidProductCategory = GdnUUIDHelper.generateUUID();
    String uuid2 = GdnUUIDHelper.generateUUID();

    this.category.setId(uuid);
    productCategory.setId(uuidProductCategory);
    categoryAttribute.setId(uuid2);
    this.category.getProductCategories().add(productCategory);
    this.category.getCategoryAttributes().add(categoryAttribute);

    this.categoryReference.setId(CategoryServiceTest.CATEGORY1_CODE);
    List<CategoryReference> categoryReferenceList = new ArrayList<CategoryReference>();
    categoryReferenceList.add(this.categoryReference);

    BeanUtils.copyProperties(this.category, savedCategory, "categoryAttributes",
        "productCategories");
    savedProductCategory.setId(uuidProductCategory);
    savedCategoryAttribute.setId(uuid2);
    savedCategory.getProductCategories().add(savedProductCategory);
    savedCategory.getCategoryAttributes().add(savedCategoryAttribute);
    savedCategory.setMasterCategoryReferences(categoryReferenceList);
    savedCategory.setSalesCategoryReferences(categoryReferenceList);

    Mockito
        .when(categoryRepository.findByStoreIdAndIdAndMarkForDeleteFalse(CategoryServiceTest.STORE_ID, uuid))
        .thenReturn(savedCategory);
    Mockito.when(this.productCategoryRepository
        .findByStoreIdAndIdAndMarkForDeleteFalse(CategoryServiceTest.STORE_ID, uuidProductCategory))
        .thenReturn(savedProductCategory);
    Mockito.when(this.categoryAttributeRepository.findByIdAndMarkForDeleteFalse(uuid2))
        .thenReturn(savedCategoryAttribute);

    Mockito.when(this.categoryRepository.findById(uuid)).thenReturn(Optional.of(savedCategory));
    Mockito.when(this.productCategoryRepository.findById(uuidProductCategory))
        .thenReturn(Optional.of(savedProductCategory));
    Mockito.when(this.categoryAttributeRepository.findById(uuid2))
        .thenReturn(Optional.of(savedCategoryAttribute));

    Mockito
        .when(this.categoryReferenceRepository.findByStoreIdAndIdAndMarkForDeleteFalse(
            CategoryServiceTest.STORE_ID, CategoryServiceTest.CATEGORY1_CODE))
        .thenReturn(this.categoryReference);
    Mockito.when(this.categoryReferenceRepository.findById(CategoryServiceTest.CATEGORY1_CODE))
        .thenReturn(Optional.of(this.categoryReference));
    Mockito.when(applicationContext.getBean(CategoryService.class)).thenReturn(categoryService);

    this.categoryServiceBean.markForDeleteCategory(CategoryServiceTest.STORE_ID,
        this.category.getId());

    assertTrue(savedCategory.isMarkForDelete());
    verify(categoryRepository, times(1))
        .findByStoreIdAndIdAndMarkForDeleteFalse(CategoryServiceTest.STORE_ID, uuid);
    verify(this.categoryRepository, times(1)).findById(uuid);
    verify(this.categoryRepository, times(1)).saveAndFlush(any(Category.class));

    assertTrue(savedProductCategory.isMarkForDelete());
    verify(this.productCategoryRepository, times(1))
        .findByStoreIdAndIdAndMarkForDeleteFalse(CategoryServiceTest.STORE_ID, uuidProductCategory);
    verify(this.productCategoryRepository, times(1)).findById(uuidProductCategory);
    verify(this.productCategoryRepository, times(1)).saveAndFlush(any(ProductCategory.class));

    assertTrue(savedCategoryAttribute.isMarkForDelete());
    verify(this.categoryAttributeRepository, times(1)).findByIdAndMarkForDeleteFalse(uuid2);
    verify(this.categoryAttributeRepository, times(1)).findById(uuid2);
    verify(this.categoryAttributeRepository, times(1)).saveAndFlush(any(CategoryAttribute.class));
    verify(categoryRepository).findByStoreIdAndParentCategoryIdAndMarkForDeleteFalse(STORE_ID, category.getId());
  }

  @Test
  public void testMarkForDeleteCategoryEmptySaleCategory() throws Exception {
    Category savedCategory = new Category();
    ProductCategory productCategory =
        new ProductCategory(new Product(), this.category, CategoryServiceTest.STORE_ID);
    ProductCategory savedProductCategory =
        new ProductCategory(new Product(), savedCategory, CategoryServiceTest.STORE_ID);
    CategoryAttribute categoryAttribute = new CategoryAttribute(savedCategory, new Attribute(),
        Integer.valueOf(0), true, true, CategoryServiceTest.STORE_ID);
    CategoryAttribute savedCategoryAttribute = new CategoryAttribute(savedCategory, new Attribute(),
        Integer.valueOf(0), true, true, CategoryServiceTest.STORE_ID);

    String uuid = GdnUUIDHelper.generateUUID();
    String uuidProductCategory = GdnUUIDHelper.generateUUID();
    String uuid2 = GdnUUIDHelper.generateUUID();

    this.category.setId(uuid);
    productCategory.setId(uuidProductCategory);
    categoryAttribute.setId(uuid2);
    this.category.getProductCategories().add(productCategory);
    this.category.getCategoryAttributes().add(categoryAttribute);

    this.categoryReference.setId(CategoryServiceTest.CATEGORY1_CODE);
    List<CategoryReference> categoryReferenceList = new ArrayList<CategoryReference>();
    categoryReferenceList.add(this.categoryReference);

    BeanUtils.copyProperties(this.category, savedCategory, "categoryAttributes",
        "productCategories");
    savedProductCategory.setId(uuidProductCategory);
    savedCategoryAttribute.setId(uuid2);
    savedCategory.getProductCategories().add(savedProductCategory);
    savedCategory.getCategoryAttributes().add(savedCategoryAttribute);
    savedCategory.setMasterCategoryReferences(categoryReferenceList);
    savedCategory.setSalesCategoryReferences(new ArrayList<>());

    Mockito
        .when(categoryRepository.findByStoreIdAndIdAndMarkForDeleteFalse(CategoryServiceTest.STORE_ID, uuid))
        .thenReturn(savedCategory);
    Mockito.when(this.productCategoryRepository
        .findByStoreIdAndIdAndMarkForDeleteFalse(CategoryServiceTest.STORE_ID, uuidProductCategory))
        .thenReturn(savedProductCategory);
    Mockito.when(this.categoryAttributeRepository.findByIdAndMarkForDeleteFalse(uuid2))
        .thenReturn(savedCategoryAttribute);

    Mockito.when(this.categoryRepository.findById(uuid)).thenReturn(Optional.of(savedCategory));
    Mockito.when(this.productCategoryRepository.findById(uuidProductCategory))
        .thenReturn(Optional.of(savedProductCategory));
    Mockito.when(this.categoryAttributeRepository.findById(uuid2))
        .thenReturn(Optional.of(savedCategoryAttribute));

    Mockito
        .when(this.categoryReferenceRepository.findByStoreIdAndIdAndMarkForDeleteFalse(
            CategoryServiceTest.STORE_ID, CategoryServiceTest.CATEGORY1_CODE))
        .thenReturn(this.categoryReference);
    Mockito.when(this.categoryReferenceRepository.findById(CategoryServiceTest.CATEGORY1_CODE))
        .thenReturn(Optional.of(this.categoryReference));
    Mockito.when(applicationContext.getBean(CategoryService.class)).thenReturn(categoryService);

    this.categoryServiceBean.markForDeleteCategory(CategoryServiceTest.STORE_ID,
        this.category.getId());

    assertTrue(savedCategory.isMarkForDelete());
    verify(categoryRepository, times(1))
        .findByStoreIdAndIdAndMarkForDeleteFalse(CategoryServiceTest.STORE_ID, uuid);
    verify(this.categoryRepository, times(1)).findById(uuid);
    verify(this.categoryRepository, times(1)).saveAndFlush(any(Category.class));

    assertTrue(savedProductCategory.isMarkForDelete());
    verify(this.productCategoryRepository, times(1))
        .findByStoreIdAndIdAndMarkForDeleteFalse(CategoryServiceTest.STORE_ID, uuidProductCategory);
    verify(this.productCategoryRepository, times(1)).findById(uuidProductCategory);
    verify(this.productCategoryRepository, times(1)).saveAndFlush(any(ProductCategory.class));

    assertTrue(savedCategoryAttribute.isMarkForDelete());
    verify(this.categoryAttributeRepository, times(1)).findByIdAndMarkForDeleteFalse(uuid2);
    verify(this.categoryAttributeRepository, times(1)).findById(uuid2);
    verify(this.categoryAttributeRepository, times(1)).saveAndFlush(any(CategoryAttribute.class));
    verify(categoryRepository).findByStoreIdAndParentCategoryIdAndMarkForDeleteFalse(STORE_ID, category.getId());
  }

  @Test
  public void testMarkForDeleteCategoryNull() throws Exception {
    Category savedCategory = new Category();
    ProductCategory productCategory =
        new ProductCategory(new Product(), this.category, CategoryServiceTest.STORE_ID);
    ProductCategory savedProductCategory =
        new ProductCategory(new Product(), savedCategory, CategoryServiceTest.STORE_ID);
    CategoryAttribute categoryAttribute = new CategoryAttribute(savedCategory, new Attribute(),
        Integer.valueOf(0), true, true, CategoryServiceTest.STORE_ID);
    CategoryAttribute savedCategoryAttribute = new CategoryAttribute(savedCategory, new Attribute(),
        Integer.valueOf(0), true, true, CategoryServiceTest.STORE_ID);

    String uuid = GdnUUIDHelper.generateUUID();
    String uuidProductCategory = GdnUUIDHelper.generateUUID();
    String uuid2 = GdnUUIDHelper.generateUUID();

    this.category.setId(uuid);
    productCategory.setId(uuidProductCategory);
    categoryAttribute.setId(uuid2);
    this.category.getProductCategories().add(productCategory);
    this.category.getCategoryAttributes().add(categoryAttribute);

    BeanUtils.copyProperties(this.category, savedCategory, "categoryAttributes",
        "productCategories");
    savedProductCategory.setId(uuidProductCategory);
    savedCategoryAttribute.setId(uuid2);
    savedCategory.getProductCategories().add(savedProductCategory);
    savedCategory.getCategoryAttributes().add(savedCategoryAttribute);
    savedCategory.setMasterCategoryReferences(new ArrayList<>());
    savedCategory.setSalesCategoryReferences(new ArrayList<>());
    savedCategory.setId(category.getId());

    Mockito
        .when(categoryRepository
            .findByStoreIdAndIdAndMarkForDeleteFalse(CategoryServiceTest.STORE_ID, uuid))
        .thenReturn(savedCategory);
    Mockito.when(this.productCategoryRepository
        .findByStoreIdAndIdAndMarkForDeleteFalse(CategoryServiceTest.STORE_ID, uuidProductCategory))
        .thenReturn(savedProductCategory);
    Mockito.when(this.categoryAttributeRepository.findByIdAndMarkForDeleteFalse(uuid2))
        .thenReturn(savedCategoryAttribute);

    Mockito.when(this.categoryRepository.findById(uuid)).thenReturn(Optional.of(savedCategory));
    Mockito.when(this.productCategoryRepository.findById(uuidProductCategory))
        .thenReturn(Optional.of(savedProductCategory));
    Mockito.when(this.categoryAttributeRepository.findById(uuid2))
        .thenReturn(Optional.of(savedCategoryAttribute));

    this.categoryServiceBean.markForDeleteCategory(CategoryServiceTest.STORE_ID,
        this.category.getId());

    assertTrue(savedCategory.isMarkForDelete());
    verify(categoryRepository, times(1))
        .findByStoreIdAndIdAndMarkForDeleteFalse(CategoryServiceTest.STORE_ID, uuid);
    verify(this.categoryRepository, times(1)).findById(uuid);
    verify(this.categoryRepository, times(1)).saveAndFlush(any(Category.class));

    assertTrue(savedProductCategory.isMarkForDelete());
    verify(this.productCategoryRepository, times(1))
        .findByStoreIdAndIdAndMarkForDeleteFalse(CategoryServiceTest.STORE_ID, uuidProductCategory);
    verify(this.productCategoryRepository, times(1)).findById(uuidProductCategory);
    verify(this.productCategoryRepository, times(1)).saveAndFlush(any(ProductCategory.class));

    assertTrue(savedCategoryAttribute.isMarkForDelete());
    verify(this.categoryAttributeRepository, times(1)).findByIdAndMarkForDeleteFalse(uuid2);
    verify(this.categoryAttributeRepository, times(1)).findById(uuid2);
    verify(this.categoryAttributeRepository, times(1)).saveAndFlush(any(CategoryAttribute.class));
    verify(categoryRepository).findByStoreIdAndParentCategoryIdAndMarkForDeleteFalse(STORE_ID, category.getId());
  }


  @Test
  public void testMarkForDeleteCategoryAttribute() throws Exception {
    CategoryAttribute categoryAttribute = new CategoryAttribute(new Category(), new Attribute(),
        Integer.valueOf(1), false, false, CategoryServiceTest.STORE_ID);
    CategoryAttribute savedCategoryAttribute = new CategoryAttribute();
    String uuid = GdnUUIDHelper.generateUUID();
    categoryAttribute.setId(uuid);
    BeanUtils.copyProperties(categoryAttribute, savedCategoryAttribute);

    Mockito
        .when(this.categoryAttributeRepository
            .findByIdAndMarkForDeleteFalse(categoryAttribute.getId()))
        .thenReturn(savedCategoryAttribute);
    Mockito.when(this.categoryAttributeRepository.findById(categoryAttribute.getId()))
        .thenReturn(Optional.of(savedCategoryAttribute));

    this.categoryServiceBean.markForDeleteCategoryAttribute(categoryAttribute.getId());

    assertTrue(savedCategoryAttribute.isMarkForDelete());
    verify(this.categoryAttributeRepository, times(1))
        .findByIdAndMarkForDeleteFalse(categoryAttribute.getId());
    verify(this.categoryAttributeRepository, times(1)).findById(categoryAttribute.getId());
    verify(this.categoryAttributeRepository, times(1)).saveAndFlush(any(CategoryAttribute.class));
  }

  @Test
  public void testMarkForDeleteCategoryAttributeWithEmptyCategoryId() {
    String id = UUID.randomUUID().toString();
    Mockito.when(this.categoryAttributeRepository.findByIdAndMarkForDeleteFalse(id))
        .thenReturn(null);
    try {
      this.categoryServiceBean.markForDeleteCategoryAttribute(id);
    } catch (Exception e) {
      assertTrue(e instanceof ApplicationException);
      assertTrue(e.getMessage().contains("Can not perform delete on un exist data"));
      verify(this.categoryAttributeRepository).findByIdAndMarkForDeleteFalse(id);
    }
  }

  @Test
  public void testMarkForDeleteCategoryReferenceSingle() throws Exception {
    String id = UUID.randomUUID().toString();
    this.categoryReference.setId(id);
    this.categoryReference.setStoreId(CategoryServiceTest.STORE_ID);
    List<CategoryReference> categoryReferenceList = new ArrayList<CategoryReference>();
    categoryReferenceList.add(this.categoryReference);
    Mockito
        .when(this.categoryReferenceRepository
            .findByStoreIdAndIdAndMarkForDeleteFalse(CategoryServiceTest.STORE_ID, id))
        .thenReturn(this.categoryReference);
    Mockito.when(this.categoryReferenceRepository.findById(id)).thenReturn(Optional.of(this.categoryReference));
    this.categoryServiceBean.markForDeleteCategoryReference(CategoryServiceTest.STORE_ID,
        categoryReferenceList);
    assertTrue(this.categoryReference.isMarkForDelete());
    verify(this.categoryReferenceRepository)
        .findByStoreIdAndIdAndMarkForDeleteFalse(CategoryServiceTest.STORE_ID, id);
    verify(this.categoryReferenceRepository).findById(id);
  }

  @Test
  public void testMarkForDeleteCategoryReferenceWithEmptyCategoryReference() throws Exception {
    String id = UUID.randomUUID().toString();
    Mockito
        .when(this.categoryReferenceRepository
            .findByStoreIdAndIdAndMarkForDeleteFalse(CategoryServiceTest.STORE_ID, id))
        .thenReturn(null);
    try {
      this.categoryServiceBean.markForDeleteCategoryReference(CategoryServiceTest.STORE_ID, id);
    } catch (Exception e) {
      assertTrue(e instanceof ApplicationException);
      assertTrue(e.getMessage().contains("Can not perform delete on un exist data"));
      verify(this.categoryReferenceRepository)
          .findByStoreIdAndIdAndMarkForDeleteFalse(CategoryServiceTest.STORE_ID, id);
    }
  }

  @Test
  public void testMarkForDeleteCategoryWithEmptyCategoryId() throws Exception {
    String id = UUID.randomUUID().toString();
    Mockito
        .when(categoryRepository.findByStoreIdAndIdAndMarkForDeleteFalse(CategoryServiceTest.STORE_ID, id))
        .thenReturn(null);
    try {
      Assertions.assertThrows(ApplicationException.class, () -> this.categoryServiceBean.markForDeleteCategory(CategoryServiceTest.STORE_ID, id));
    } finally {
      verify(categoryRepository).findByStoreIdAndIdAndMarkForDeleteFalse(CategoryServiceTest.STORE_ID, id);
    }
  }

  @Test
  public void testMarkForDeleteProductCategory() throws Exception {
    ProductCategory productCategory =
        new ProductCategory(new Product(), new Category(), CategoryServiceTest.STORE_ID);
    ProductCategory savedProductCategory = new ProductCategory();
    String uuid = GdnUUIDHelper.generateUUID();
    productCategory.setId(uuid);
    BeanUtils.copyProperties(productCategory, savedProductCategory);

    Mockito
        .when(this.productCategoryRepository.findByStoreIdAndIdAndMarkForDeleteFalse(
            CategoryServiceTest.STORE_ID, productCategory.getId()))
        .thenReturn(savedProductCategory);
    Mockito.when(this.productCategoryRepository.findById(productCategory.getId()))
        .thenReturn(Optional.of(savedProductCategory));

    this.categoryServiceBean.markForDeleteProductCategory(CategoryServiceTest.STORE_ID,
        productCategory.getId());

    assertTrue(savedProductCategory.isMarkForDelete());
    verify(this.productCategoryRepository, times(1))
        .findByStoreIdAndIdAndMarkForDeleteFalse(CategoryServiceTest.STORE_ID,
            productCategory.getId());
    verify(this.productCategoryRepository, times(1)).findById(productCategory.getId());
    verify(this.productCategoryRepository, times(1)).saveAndFlush(any(ProductCategory.class));
  }

  @Test
  public void testMarkForDeleteProductCategoryWithEmptyProductCategory() throws Exception {

    String uuid = GdnUUIDHelper.generateUUID();
    try {
      Mockito
          .when(this.productCategoryRepository
              .findByStoreIdAndIdAndMarkForDeleteFalse(CategoryServiceTest.STORE_ID, uuid))
          .thenReturn(null);

      this.categoryServiceBean.markForDeleteProductCategory(CategoryServiceTest.STORE_ID, uuid);
    } catch (Exception e) {
      assertTrue(e instanceof ApplicationException);
      assertTrue(e.getMessage().contains("Can not perform delete on un exist data"));
    }
    verify(this.productCategoryRepository, times(1))
        .findByStoreIdAndIdAndMarkForDeleteFalse(CategoryServiceTest.STORE_ID, uuid);
  }

  @Test
  public void testSaveAndUpdateProductCategorySuccessfully() throws Exception {
    this.category.setCategoryCode("CA-1000001");
    Mockito.when(this.categoryRepository.saveAndFlush(this.category)).thenReturn(this.category);

    this.categoryServiceBean.saveAndUpdateProductCategory(CategoryServiceTest.STORE_ID,
        this.category);

    verify(this.categoryRepository, times(1)).saveAndFlush(this.category);
  }

  @Test
  public void testSaveAndUpdateProductCategoryWithGenericTemplateEligibleTrue() throws Exception {
    this.category.setCategoryCode(CATEGORY_CODE);
    category.setGenericTemplateEligible(true);
    Mockito.when(this.categoryRepository.saveAndFlush(this.category)).thenReturn(this.category);

    this.categoryServiceBean.saveAndUpdateProductCategory(CategoryServiceTest.STORE_ID, this.category);

    verify(this.categoryRepository, times(1)).saveAndFlush(categoryArgumentCaptor.capture());
    assertTrue(categoryArgumentCaptor.getValue().isGenericTemplateEligible());
  }

  @Test
  public void testSaveAndUpdateProductCategoryNotEmpty() throws Exception {
    this.categoryReference.setId(CategoryServiceTest.CATEGORY1_CODE);
    this.categoryReference.setMasterCategory(this.category);
    List<CategoryReference> categoryReferenceList = new ArrayList<CategoryReference>();
    categoryReferenceList.add(this.categoryReference);
    this.category.setCategoryCode("");
    this.category.setName(CATEGORY1_NAME);
    this.category.setMasterCategoryReferences(categoryReferenceList);
    Catalog catalog = new Catalog();
    catalog.setCatalogType(CatalogType.MASTER_CATALOG);
    this.category.setCatalog(catalog);
    ProductCategory productCategory =
        new ProductCategory(new Product(), this.category, CategoryServiceTest.STORE_ID);
    productCategory.setProduct(new Product());
    List<ProductCategory> listProductCategory = new ArrayList<ProductCategory>();
    listProductCategory.add(productCategory);

    Mockito.when(this.categoryRepository.getSequenceByCategoryCode(Mockito.any()))
        .thenReturn(5L);
    Mockito.when(this.categoryRepository.saveAndFlush(this.category)).thenReturn(this.category);
    Mockito.when(this.productCategoryRepository.saveAndFlush(productCategory))
        .thenReturn(productCategory);
    Mockito.when(this.productCategoryRepository
        .findByStoreIdAndCategoryIdAndMarkForDeleteFalse(Mockito.any(), Mockito.any()))
        .thenReturn(listProductCategory);

    this.categoryServiceBean.saveAndUpdateProductCategory(CategoryServiceTest.STORE_ID,
        this.category);

    verify(this.categoryRepository, times(1)).saveAndFlush(this.category);
    verify(this.categoryRepository, times(1))
        .getSequenceByCategoryCode(Mockito.anyString());
    verify(this.productCategoryRepository, times(1)).saveAndFlush(productCategory);
    verify(this.productCategoryRepository, times(1))
        .findByStoreIdAndCategoryIdAndMarkForDeleteFalse(Mockito.any(), Mockito.any());
  }

  @Test
  public void testSaveAndUpdateProductCategoryNotEmptyAndSalesCatalog() throws Exception {
    this.categoryReference.setId(CategoryServiceTest.CATEGORY1_CODE);
    this.categoryReference.setMasterCategory(this.category);
    List<CategoryReference> categoryReferenceList = new ArrayList<CategoryReference>();
    categoryReferenceList.add(this.categoryReference);
    this.category.setCategoryCode("");
    this.category.setName(CATEGORY1_NAME);
    this.category.setMasterCategoryReferences(categoryReferenceList);
    Catalog catalog = new Catalog();
    catalog.setCatalogType(CatalogType.SALES_CATALOG);
    this.category.setCatalog(catalog);
    ProductCategory productCategory =
        new ProductCategory(new Product(), this.category, CategoryServiceTest.STORE_ID);
    productCategory.setProduct(new Product());
    List<ProductCategory> listProductCategory = new ArrayList<ProductCategory>();
    listProductCategory.add(productCategory);

    Mockito.when(this.categoryRepository.getSequenceByCategoryCode(Mockito.anyString()))
        .thenReturn(5L);
    Mockito.when(this.categoryRepository.saveAndFlush(this.category)).thenReturn(this.category);
    Mockito.when(this.productCategoryRepository.saveAndFlush(productCategory))
        .thenReturn(productCategory);

    this.categoryServiceBean.saveAndUpdateProductCategory(CategoryServiceTest.STORE_ID,
        this.category);

    verify(this.categoryRepository, times(1)).saveAndFlush(this.category);
    verify(this.categoryRepository, times(1))
        .getSequenceByCategoryCode(Mockito.anyString());
    verify(this.productCategoryRepository, times(0)).saveAndFlush(productCategory);
    verify(this.productCategoryRepository, times(0))
        .findByStoreIdAndCategoryIdAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void setCategoryDisplayableTest() throws Exception {
    Mockito.when(categoryService.getCategoryByStoreIdAndIdCached(STORE_ID, CATEGORY_ID))
        .thenReturn(category);
    Mockito.when(applicationContext.getBean(CategoryService.class)).thenReturn(categoryService);
    this.categoryServiceBean.setCategoryDisplayable(CATEGORY_ID, false);

    verify(applicationContext).getBean(CategoryService.class);
    verify(categoryService).getCategoryByStoreIdAndIdCached(STORE_ID, CATEGORY_ID);
    verify(categoryRepository).save(category);
    applicationCacheServiceBean.evictCategoryCacheByStoreIdAndCategoryId(STORE_ID, CATEGORY_ID);
    applicationCacheServiceBean.evictCategoryCacheByStoreIdAndCategoryId(
        STORE_ID, category.getCategoryCode());
    verify(categoryRepository).findByStoreIdAndParentCategoryIdAndMarkForDeleteFalse(STORE_ID, category.getId());
  }

  @Test
  public void testSavingCategorySuccessfully() throws Exception {
    this.category.setCategoryCode("CA-1000001");
    Category savedCategory = new Category();
    BeanUtils.copyProperties(this.category, savedCategory);
    String uuid = GdnUUIDHelper.generateUUID();
    savedCategory.setId(uuid);

    Mockito.when(this.categoryRepository.save(this.category)).thenReturn(savedCategory);
    Category updatedCategory = categoryServiceBean.save(this.category);
    assertEquals(uuid, updatedCategory.getId());
    verify(this.categoryRepository, times(1)).save(this.category);
  }

  @Test
  public void testSavingCategoryWithoutCategoryCodeSuccessfully() throws Exception {
    Category savedCategory = new Category();
    BeanUtils.copyProperties(this.category, savedCategory);
    String uuid = GdnUUIDHelper.generateUUID();
    savedCategory.setId(uuid);

    Mockito.when(this.categoryRepository.getSequenceByCategoryCode("CA")).thenReturn(1L);
    Mockito.when(this.categoryRepository.save(this.category)).thenReturn(savedCategory);
    Category updatedCategory = this.categoryServiceBean.save(this.category);
    assertEquals(updatedCategory.getId(), uuid);
    verify(this.categoryRepository).getSequenceByCategoryCode("CA");
    verify(this.categoryRepository, times(1)).save(this.category);
  }

  @Test
  public void testUpdateCategorySuccessfully() throws Exception {
    String uuid = GdnUUIDHelper.generateUUID();
    this.category.setId(uuid);
    Category parentCategory = new Category();
    this.category.setParentCategory(parentCategory);
    Mockito.when(this.categoryRepository.findById(uuid)).thenReturn(Optional.of(this.category));
    Mockito.when(this.categoryRepository.saveAndFlush(this.category)).thenReturn(this.category);
    this.categoryServiceBean.update(this.category);

    verify(this.categoryRepository).findById(uuid);
    verify(this.categoryRepository, times(1)).saveAndFlush(this.category);
  }

  @Test
  public void testFindByCategoryNameAndCatalogType() throws Exception {
    this.categoryServiceBean.findByCategoryNameAndCatalogType(STORE_ID, "OMA",
        CatalogType.MASTER_CATALOG, DEFAULT_PAGEABLE);
    verify(this.categoryRepository)
        .findByStoreIdAndCategoryNameAndCatalogTypeAndMarkForDeleteFalse(Mockito.anyString(),
            Mockito.anyString(), (CatalogType) any(), (Pageable) any());
  }

  @Test
  public void testGetCategoriesFromCatalogType() throws Exception {
    CustomCategoryDto customCategoryEntity = new CustomCategoryDto();
    customCategoryEntity.setId(ID);
    List<CustomCategoryDto> customCategoryEntityList = new ArrayList<>();
    customCategoryEntityList.add(customCategoryEntity);
    Mockito
        .when(this.categoryDAO.getCategoriesFromCatalogType(STORE_ID, CatalogType.MASTER_CATALOG))
        .thenReturn(customCategoryEntityList);
    List<CustomCategoryDto> categoryList =
        this.categoryServiceBean.getCategoriesFromCatalogType(STORE_ID, CatalogType.MASTER_CATALOG);
    assertEquals(ID, categoryList.get(0).getId());
    verify(this.categoryDAO).getCategoriesFromCatalogType(STORE_ID,
        CatalogType.MASTER_CATALOG);
  }

  @Test
  public void testGetParentCategories_categoryExist_returnParentCategories() {
    List<String> value = new ArrayList<>();
    value.add(CATEGORY_ID);
    Mockito.when(this.categoryRepository.getParentCategories()).thenReturn(value);
    List<String> parentCategories = this.categoryServiceBean.getParentCategories();
    assertFalse(parentCategories.isEmpty());
    verify(this.categoryRepository).getParentCategories();
  }

  @Test
  public void testGetParentCategories_NoParentExist_returnEmptyList() {
    List<String> value = new ArrayList<>();
    value.add(CATEGORY_ID);
    Mockito.when(this.categoryRepository.getParentCategories()).thenReturn(null);
    List<String> parentCategories = this.categoryServiceBean.getParentCategories();
    assertTrue(parentCategories.isEmpty());
    verify(this.categoryRepository).getParentCategories();
  }

  @Test
  public void getFinalParentCategory() throws Exception {
    Mockito.when(this.categoryRepository.getFinalParentCategoryId(CATEGORY_ID))
        .thenReturn(CATEGORY_ID);

    this.categoryServiceBean.getFinalParentCategory(CATEGORY_ID);

    verify(this.categoryRepository).getFinalParentCategoryId(CATEGORY_ID);
  }

  @Test
  public void getParentCategoryHierarchyByCategoryIdTest() throws Exception {
    Mockito.when(this.categoryRepository.getParentCategoryHierarchyByCategoryId(CATEGORY_ID))
        .thenReturn(Arrays.asList(CATEGORY_ID));
    this.categoryServiceBean.getParentCategoryHierarchyByCategoryId(CATEGORY_ID);
    verify(this.categoryRepository).getParentCategoryHierarchyByCategoryId(CATEGORY_ID);
  }

  @Test
  public void findByStoreId() throws Exception {
    List<Category> categoryList = new ArrayList<Category>();
    Mockito.when(this.categoryRepository.findByStoreIdAndMarkForDeleteFalse(STORE_ID))
        .thenReturn(categoryList);

    this.categoryServiceBean.findByStoreId(STORE_ID);

    verify(this.categoryRepository).findByStoreIdAndMarkForDeleteFalse(STORE_ID);
  }

  @Test
  public void getAllCategoryTreeTest() throws Exception {
    Mockito.when(this.categoryRepository.getAllCategoryByCatalogName(Mockito.anyString(),
        Mockito.anyString())).thenReturn(this.catDTOList);
    Mockito.when(this.catalogRepository.findByStoreIdAndNameStartingWithAndMarkForDeleteFalse(
        Mockito.anyString(), Mockito.anyString())).thenReturn(this.catalogList);

    this.categoryServiceBean.getAllCategoryTree(CATALOG_NAME, STORE_ID, false);

    verify(this.catalogRepository).findByStoreIdAndNameStartingWithAndMarkForDeleteFalse(
        Mockito.anyString(), Mockito.anyString());
    verify(this.categoryRepository).getAllCategoryByCatalogName(Mockito.anyString(),
        Mockito.anyString());
  }

  @Test
  public void getAllNonB2bExclusiveCategoryTreeTest() throws Exception {
    this.catDTOList.add(new CategoryTreeDTO("ID", CATEGORY2_CODE, "Test", "Test", "", true, "", true));
    Mockito.when(this.categoryRepository.getAllCategoryByCatalogName(STORE_ID, "C-001")).thenReturn(this.catDTOList);
    Mockito.when(this.catalogRepository.findByStoreIdAndNameStartingWithAndMarkForDeleteFalse(STORE_ID, CATALOG_NAME))
        .thenReturn(this.catalogList);

    List<CategoryTreeDTO> responseTree = this.categoryServiceBean.getAllCategoryTree(CATALOG_NAME, STORE_ID, true);

    verify(this.catalogRepository).findByStoreIdAndNameStartingWithAndMarkForDeleteFalse(STORE_ID, CATALOG_NAME);
    verify(this.categoryRepository).getAllCategoryByCatalogName(STORE_ID, "C-001");
    assertEquals(5,responseTree.size());
  }

  @Test
  public void getAllActiveCategoryTreeTest() throws Exception {
    Mockito.when(this.categoryRepository
        .getAllCategoryByCatalogId(eq(STORE_ID), eq(catalogList.get(0).getId()))).thenReturn(this.catDTOList);
    Mockito.when(this.catalogRepository
        .findByStoreIdAndNameStartingWithAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(this.catalogList);
    final List<CategoryTreeDTO> allCategoryTree =
        this.categoryServiceBean.getActiveCategoryTree(CATALOG_NAME, STORE_ID);
    verify(this.catalogRepository)
        .findByStoreIdAndNameStartingWithAndMarkForDeleteFalse(eq(STORE_ID), eq(CATALOG_NAME));
    verify(this.categoryRepository)
        .getAllCategoryByCatalogId(eq(STORE_ID), eq(catalogList.get(0).getId()));
    assertEquals(7, allCategoryTree.size());
  }

  @Test
  public void buildCategoryTreeTest() throws Exception {
    List<CategoryTreeDTO> allCategoryTree = this.categoryServiceBean.buildCategoryTree(catDTOList, StringUtils.EMPTY);
    assertEquals(5, allCategoryTree.size());
  }

  @Test
  public void getAllCategoryTreeWIthNullCatalogName() throws Exception {
    Mockito.when(this.categoryRepository.getAllCategoryByCatalogName(Mockito.anyString(),
        Mockito.anyString())).thenReturn(this.catDTOList);
    Mockito.when(this.catalogRepository.findByStoreIdAndNameStartingWithAndMarkForDeleteFalse(
        Mockito.anyString(), Mockito.anyString())).thenReturn(this.catalogList);

    this.categoryServiceBean.getAllCategoryTree("", STORE_ID, false);

    verify(this.catalogRepository).findByStoreIdAndNameStartingWithAndMarkForDeleteFalse(
        Mockito.anyString(), Mockito.anyString());
    verify(this.categoryRepository).getAllCategoryByCatalogName(Mockito.anyString(),
        Mockito.anyString());
  }

  @Test
  public void getAllCategoryTreeWHenError() throws Exception {
    Mockito.when(this.catalogRepository.findByStoreIdAndNameStartingWithAndMarkForDeleteFalse(
        Mockito.anyString(), Mockito.anyString())).thenThrow(RuntimeException.class);

    try {
      this.categoryServiceBean.getAllCategoryTree(CATEGORY1_NAME, STORE_ID, false);
    } catch (Exception e) {
      verify(this.catalogRepository).findByStoreIdAndNameStartingWithAndMarkForDeleteFalse(
          Mockito.anyString(), Mockito.anyString());
    }
  }

  @Test
  public void getCategoryTreeTest() throws Exception {
    Mockito.when(this.categoryRepository.getCategoryByParentIdNative(Mockito.anyString(),
        Mockito.anyString(), Mockito.anyList())).thenReturn(this.catList);
    Mockito.when(this.catalogRepository.findByStoreIdAndNameStartingWithAndMarkForDeleteFalse(
        Mockito.anyString(), Mockito.anyString())).thenReturn(this.catalogList);

    List<String> categoryCodes = new ArrayList<>();
    categoryCodes.add(CATEGORY2_CODE);

    this.categoryServiceBean.getCategoryTree(categoryCodes, "", STORE_ID);

    verify(this.categoryRepository).getCategoryByParentIdNative(Mockito.anyString(),
        Mockito.anyString(), Mockito.anyList());
    verify(this.catalogRepository).findByStoreIdAndNameStartingWithAndMarkForDeleteFalse(
        Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void getCategoryTreeWhenError() throws Exception {
    Mockito.when(this.categoryRepository.getCategoryByParentIdNative(Mockito.anyString(),
        Mockito.anyString(), Mockito.anyList())).thenThrow(RuntimeException.class);

    List<String> categoryCodes = new ArrayList<>();
    categoryCodes.add(CATEGORY2_CODE);

    Assertions.assertThrows(Exception.class, () -> this.categoryServiceBean.getCategoryTree(categoryCodes, CATALOG_NAME, STORE_ID));

  }

  @Test
  public void getCategoryToFinalParentTest() throws Exception {
    Mockito.when(this.categoryRepository.getCategoryAndParentCategories()).thenReturn(this.catList);

    this.categoryServiceBean.getCategoryToFinalParent();

    verify(this.categoryRepository).getCategoryAndParentCategories();
  }

  @Test
  public void testFindCategorySummaryByStoreIdAndCatalogIdAndDisplay() {
    this.categoryServiceBean.findCategorySummaryByStoreIdAndCatalogIdAndDisplay(STORE_ID,
        CATALOG_ID, DISPLAY, DEFAULT_PAGEABLE);
    verify(this.categoryRepository).findCategorySummaryByStoreIdAndCatalogIdAndDisplay(
        STORE_ID, CATALOG_ID, DISPLAY, DEFAULT_PAGEABLE);
  }

  @Test
  public void findCategoryNamesByCategoryCodesTest() throws Exception {
    Map<String, String> response;
    Mockito
        .when(this.categoryRepository.findByStoreIdAndCategoryCodesAndMarkForDeleteFalse(STORE_ID,
            this.categoryCodes, this.defaultPageable))
        .thenReturn(new PageImpl<>(getDefaultCategoriesWithCategoryCode()));
    response = this.categoryServiceBean.findCategoryNamesByCategoryCodes(STORE_ID,
        this.categoryCodes, this.defaultPageable);
    verify(this.categoryRepository).findByStoreIdAndCategoryCodesAndMarkForDeleteFalse(
        STORE_ID, this.categoryCodes, this.defaultPageable);
    assertNotNull(response);
    assertEquals(2, response.size());
  }

  @Test
  public void findAllChildForC1CategoryCodesTest() throws Exception {
    List<Object[]> categories2 = new ArrayList<>();
    Object[] object1 = new Object[] {"10101", "CODE1"};
    categories2.add(object1);
    Mockito.when(this.categoryRepository
        .findByStoreIdAndCategoryCodes(Mockito.anyString(), Mockito.any(),
            any(PageRequest.class)))
        .thenReturn(new PageImpl<>(getCategoriesWithCategoryCode(), PageRequest.of(0, 100), 2));
    Mockito.when(this.categoryRepository.findByStoreIdAndParentCategoryId(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(categories2);
    Mockito.when(cacheServiceHelperBean.findChildCategoriesIncludingInactive(STORE_ID, CATEGORY2_CODE,
      "1")).thenReturn(Collections.singletonList("CODE1"));
    Mockito.when(cacheServiceHelperBean.findChildCategoriesIncludingInactive(STORE_ID, CATEGORY2_CODE,
      "2")).thenReturn(Collections.singletonList("10102"));
    Mockito.when(cacheServiceHelperBean.findChildCategoriesIncludingInactive(STORE_ID, CATEGORY1_CODE,
      "1")).thenReturn(Collections.singletonList("CODE1"));
    Mockito.when(cacheServiceHelperBean.findChildCategoriesIncludingInactive(STORE_ID, CATEGORY1_CODE,
      "2")).thenReturn(Collections.singletonList("10102"));
    List<String> result = this.categoryServiceBean.findAllChildForC1CategoryCodes(STORE_ID,
      this.categoryCodes, false);
    verify(this.categoryRepository).findByStoreIdAndCategoryCodes(Mockito.anyString(), Mockito.any(),
        any(PageRequest.class));
    verify(cacheServiceHelperBean).findChildCategoriesIncludingInactive(STORE_ID, CATEGORY2_CODE,
      "2");
    verify(cacheServiceHelperBean).findChildCategoriesIncludingInactive(STORE_ID, CATEGORY1_CODE,
      "1");
    assertNotNull(result);
    assertEquals("CODE1", result.get(0).toString());
  }

  @Test
  public void findAllChildForC1CategoryCodesTestWithCacheCall() throws Exception {
    List<Object[]> categories2 = new ArrayList<>();
    Object[] object1 = new Object[] {"10101", "CODE1"};
    categories2.add(object1);
    Mockito.when(this.categoryRepository.findByStoreIdAndParentCategoryId(Mockito.anyString(), Mockito.anyString()))
      .thenReturn(categories2);
    Mockito.when(cacheServiceHelperBean.findChildCategoriesIncludingInactive(STORE_ID, CATEGORY2_CODE,
      "1")).thenReturn(Collections.singletonList("10101"));
    Mockito.when(cacheServiceHelperBean.findChildCategoriesIncludingInactive(STORE_ID, CATEGORY2_CODE,
      "2")).thenReturn(Collections.singletonList("10102"));
    Mockito.when(cacheServiceHelperBean.findChildCategoriesIncludingInactive(STORE_ID, CATEGORY1_CODE,
      "1")).thenReturn(Collections.singletonList("10101"));
    Mockito.when(cacheServiceHelperBean.findChildCategoriesIncludingInactive(STORE_ID, CATEGORY1_CODE,
      "2")).thenReturn(Collections.singletonList("10102"));
    Mockito.when(this.categoryRepository
        .findByStoreIdAndCategoryCodes(Mockito.anyString(), Mockito.any(),
          any(PageRequest.class)))
      .thenReturn(new PageImpl<>(getCategoriesWithCategoryCode(), PageRequest.of(0, 100), 2));
    List<String> result = this.categoryServiceBean.findAllChildForC1CategoryCodes(STORE_ID,
      this.categoryCodes, false);
    verify(this.categoryRepository).findByStoreIdAndCategoryCodes(Mockito.anyString(), Mockito.any(),
      any(PageRequest.class));
    verify(this.categoryRepository).findByStoreIdAndCategoryCodes(Mockito.anyString(), Mockito.any(),
      any(PageRequest.class));
    verify(cacheServiceHelperBean).findChildCategoriesIncludingInactive(STORE_ID, CATEGORY1_CODE,
      "1");
    verify(cacheServiceHelperBean).findChildCategoriesIncludingInactive(STORE_ID, CATEGORY2_CODE,
      "2");
    assertEquals("10101", result.get(0).toString());
  }

  @Test
  public void findAllChildForC1CategoryCodesTestWithActiveCategoryCacheCall() throws Exception {
    List<Object[]> categories2 = new ArrayList<>();
    Object[] object1 = new Object[] {"10101", "10102"};
    categories2.add(object1);
    Mockito.when(cacheServiceHelperBean.findChildCategoriesActiveOnly(STORE_ID, CATEGORY2_CODE,
      "1")).thenReturn(Collections.singletonList("10102"));
    Mockito.when(cacheServiceHelperBean.findChildCategoriesActiveOnly(STORE_ID, CATEGORY2_CODE,
      "2")).thenReturn(Collections.singletonList("10102"));
    Mockito.when(cacheServiceHelperBean.findChildCategoriesActiveOnly(STORE_ID, CATEGORY1_CODE,
      "1")).thenReturn(Collections.singletonList("10102"));
    Mockito.when(cacheServiceHelperBean.findChildCategoriesActiveOnly(STORE_ID, CATEGORY1_CODE,
      "2")).thenReturn(Collections.singletonList("10102"));

    Mockito.when(this.categoryRepository
        .findByStoreIdAndCategoryCodes(Mockito.anyString(), Mockito.any(),
          any(PageRequest.class)))
      .thenReturn(new PageImpl<>(getCategoriesWithCategoryCode(), PageRequest.of(0, 100), 2));
    List<String> result = this.categoryServiceBean.findAllChildForC1CategoryCodes(STORE_ID,
      this.categoryCodes, true);
    verify(this.categoryRepository).findByStoreIdAndCategoryCodes(Mockito.anyString(), Mockito.any(),
      any(PageRequest.class));
    verify(this.categoryRepository).findByStoreIdAndCategoryCodes(Mockito.anyString(), Mockito.any(),
      any(PageRequest.class));
    verify(cacheServiceHelperBean).findChildCategoriesActiveOnly(STORE_ID, CATEGORY1_CODE,
      "1");
    verify(cacheServiceHelperBean).findChildCategoriesActiveOnly(STORE_ID, CATEGORY2_CODE,
      "2");
    assertEquals("10102", result.get(0).toString());
  }

  @Test
  public void findAllChildForC1CategoryCodesForActiveCn() throws Exception {
    List<Object[]> categories2 = new ArrayList<>();
    Object[] object1 = new Object[] {"10101", "CODE1"};
    categories2.add(object1);
    Mockito.when(this.categoryRepository
        .findByStoreIdAndCategoryCodes(Mockito.anyString(), Mockito.any(),
          any(PageRequest.class)))
      .thenReturn(new PageImpl<>(getCategoriesWithCategoryCode(), PageRequest.of(0, 100), 2));
    Mockito.when(cacheServiceHelperBean.findChildCategoriesActiveOnly(STORE_ID, CATEGORY2_CODE,
      "1")).thenReturn(Collections.singletonList("10101"));
    Mockito.when(cacheServiceHelperBean.findChildCategoriesActiveOnly(STORE_ID, CATEGORY2_CODE,
      "2")).thenReturn(Collections.singletonList("10102"));
    Mockito.when(cacheServiceHelperBean.findChildCategoriesActiveOnly(STORE_ID, CATEGORY1_CODE,
      "1")).thenReturn(Collections.singletonList("10101"));
    Mockito.when(cacheServiceHelperBean.findChildCategoriesActiveOnly(STORE_ID, CATEGORY1_CODE,
      "2")).thenReturn(Collections.singletonList("10102"));
    List<String> result = this.categoryServiceBean.findAllChildForC1CategoryCodes(STORE_ID,
      this.categoryCodes, true);
    verify(this.categoryRepository).findByStoreIdAndCategoryCodes(Mockito.anyString(), Mockito.any(),
      any(PageRequest.class));
    verify(cacheServiceHelperBean).findChildCategoriesActiveOnly(STORE_ID, CATEGORY1_CODE,
      "1");
    verify(cacheServiceHelperBean).findChildCategoriesActiveOnly(STORE_ID, CATEGORY2_CODE,
      "2");
    assertNotNull(result);
    assertEquals("10101", result.get(0).toString());
  }

  @Test
  public void findAllChildForC1CategoryCodesForActiveCnCacheFetch() throws Exception {
    List<Object[]> categories2 = new ArrayList<>();
    Object[] object1 = new Object[] {"10101", "CODE1"};
    Mockito.when(this.categoryRepository
        .findByStoreIdAndCategoryCodes(Mockito.anyString(), Mockito.any(),
          any(PageRequest.class)))
      .thenReturn(new PageImpl<>(getCategoriesWithCategoryCode(), PageRequest.of(0, 100), 2));
    Mockito.when(this.categoryRepository.findByStoreIdAndParentCategoryIdAndIsActivatedTrue(Mockito.anyString(), Mockito.anyString()))
      .thenReturn(categories2);
    Mockito.when(cacheServiceHelperBean.findChildCategoriesActiveOnly(STORE_ID, CATEGORY2_CODE,
        "1")).thenReturn(Collections.singletonList("10101"));
    Mockito.when(cacheServiceHelperBean.findChildCategoriesActiveOnly(STORE_ID, CATEGORY2_CODE,
      "2")).thenReturn(Collections.singletonList("10102"));
    Mockito.when(cacheServiceHelperBean.findChildCategoriesActiveOnly(STORE_ID, CATEGORY1_CODE,
      "1")).thenReturn(Collections.singletonList("10101"));
    Mockito.when(cacheServiceHelperBean.findChildCategoriesActiveOnly(STORE_ID, CATEGORY1_CODE,
      "2")).thenReturn(Collections.singletonList("10102"));
    List<String> result = this.categoryServiceBean.findAllChildForC1CategoryCodes(STORE_ID,
      this.categoryCodes, true);
    verify(this.categoryRepository).findByStoreIdAndCategoryCodes(Mockito.anyString(), Mockito.any(),
      any(PageRequest.class));
    verify(cacheServiceHelperBean).findChildCategoriesActiveOnly(STORE_ID, CATEGORY1_CODE,
      "1");
    verify(cacheServiceHelperBean).findChildCategoriesActiveOnly(STORE_ID, CATEGORY2_CODE,
      "2");
    assertEquals("10101", result.get(0).toString());
  }

  @Test
  public void findAllChildForC1CategoryCodesTest_emptyPageContent() throws Exception {
    Mockito.when(this.categoryRepository
        .findByStoreIdAndCategoryCodes(Mockito.anyString(), Mockito.any(),
            any(PageRequest.class)))
        .thenReturn(new PageImpl<>(new ArrayList<>(), PageRequest.of(0, 100), 0));
    List<String> result = this.categoryServiceBean.findAllChildForC1CategoryCodes(STORE_ID,
      this.categoryCodes, false);
    verify(this.categoryRepository).findByStoreIdAndCategoryCodes(Mockito.anyString(), Mockito.any(),
        any(PageRequest.class));
  }

  @Test
  public void validateIsCategoryCn_CategoryHasChild_SuccessTrue() throws Exception {
    Category cat = new Category(STORE_ID, CATEGORY1_NAME, 0);
    Mockito.when(this.categoryRepository.findByStoreIdAndParentIdAndMarkForDeleteFalse(STORE_ID,
        CATEGORY_ID)).thenReturn(Arrays.asList(cat));

    boolean result = this.categoryServiceBean.validateIsCategoryCn(STORE_ID, CATEGORY_ID);

    assertTrue(result);
    verify(this.categoryRepository).findByStoreIdAndParentIdAndMarkForDeleteFalse(STORE_ID,
        CATEGORY_ID);
  }

  @Test
  public void validateIsCategoryCn_CategoryHasChild_SuccessFalse() throws Exception {
    Category category1 = new Category(STORE_ID, CATEGORY1_NAME, 0);
    Category category2 = new Category(STORE_ID, CATEGORY1_NAME, 0);
    category2.setActivated(true);
    Mockito.when(this.categoryRepository.findByStoreIdAndParentIdAndMarkForDeleteFalse(STORE_ID,
        CATEGORY_ID)).thenReturn(Arrays.asList(category1, category2));
    boolean result = this.categoryServiceBean.validateIsCategoryCn(STORE_ID, CATEGORY_ID);
    assertFalse(result);
    verify(this.categoryRepository).findByStoreIdAndParentIdAndMarkForDeleteFalse(STORE_ID,
        CATEGORY_ID);
  }

  @Test
  public void validateIsCategoryCn_CategoryCn_SuccessTrue() throws Exception {
    Mockito.when(this.categoryRepository.findByStoreIdAndParentIdAndMarkForDeleteFalse(STORE_ID,
        CATEGORY_ID)).thenReturn(null);

    boolean result = this.categoryServiceBean.validateIsCategoryCn(STORE_ID, CATEGORY_ID);

    assertEquals(true, result);
    verify(this.categoryRepository).findByStoreIdAndParentIdAndMarkForDeleteFalse(STORE_ID,
        CATEGORY_ID);
  }

  @Test
  public void testPublishAllCategories() throws Exception {

    Stream<CategoryTreeDTO> asd = Stream.of(categoryTreeDTO1,categoryTreeDTO4);
    Mockito.when(categoryRepository.getStreamedAllCategoryByCatalogId(STORE_ID, "C-001"))
        .thenReturn(asd);

    Mockito.when(categoryService.getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(STORE_ID, CATEGORY1_CODE))
        .thenReturn(category1);
    Mockito.when(categoryService.getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(STORE_ID, CATEGORY2_CODE))
        .thenReturn(category2);
    Mockito.when(categoryService.getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(STORE_ID, CATEGORY3_CODE))
        .thenReturn(category3);
    Mockito.when(categoryService.getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(STORE_ID, CATEGORY4_CODE))
        .thenReturn(category4);

    Mockito.when(this.catalogRepository.findByStoreIdAndNameStartingWithAndMarkForDeleteFalse(Mockito.anyString(),
        Mockito.anyString()))
        .thenReturn(this.catalogList);

    Boolean result = categoryServiceBean.publishAllCategories(null, STORE_ID);
    assertTrue(result);

    Mockito.verify(categoryRepository)
        .getStreamedAllCategoryByCatalogId(STORE_ID, "C-001");

    verify(applicationContext, times(4)).getBean(CategoryService.class);
    Mockito.verify(categoryService)
        .getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(STORE_ID, CATEGORY1_CODE);
    Mockito.verify(categoryService)
        .getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(STORE_ID, CATEGORY2_CODE);
    Mockito.verify(categoryService)
        .getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(STORE_ID, CATEGORY3_CODE);
    Mockito.verify(categoryService)
        .getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(STORE_ID, CATEGORY4_CODE);

    verify(domainEventPublisherService,times(4))
        .publishAllCategory(any(Category.class));
    Mockito.verify(catalogRepository)
        .findByStoreIdAndNameStartingWithAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString());
  }

  private void initializeCategoriesPublisherMock()
  {
    categoryTreeDTO1.setCategoryCode(CATEGORY1_CODE);
    categoryTreeDTO1.setParentCategory("");

    categoryTreeDTO2.setCategoryCode(CATEGORY2_CODE);
    categoryTreeDTO2.setParentCategory(CATEGORY1_CODE);

    categoryTreeDTO3.setParentCategory(CATEGORY2_CODE);
    categoryTreeDTO3.setCategoryCode(CATEGORY3_CODE);

    categoryTreeDTO4.setParentCategory("");
    categoryTreeDTO4.setCategoryCode(CATEGORY4_CODE);

    categoryTreeDTO2.setChildren(Collections.singletonList(categoryTreeDTO3));
    categoryTreeDTO1.setChildren(Collections.singletonList(categoryTreeDTO2));
    categoryTreeDTOS.add(categoryTreeDTO1);
    categoryTreeDTOS.add(categoryTreeDTO4);

    category1.setCategoryCode(CATEGORY1_CODE);
    category1.setName(CATEGORY1_NAME);
    category1.setId(CATEGORY_ID1);

    category2.setCategoryCode(CATEGORY2_CODE);
    category2.setName(CATEGORY2_NAME);
    category2.setId(CATEGORY_ID3);

    category3.setCategoryCode(CATEGORY3_CODE);

    category4.setCategoryCode(CATEGORY4_CODE);
  }

  @Test
  public void saveUpdatedCategory() throws Exception {
    Category category = getDefaultCategory(2);
    category.setId(CategoryServiceTest.CATEGORY_ID2);
    List<CategoryReference> masterCategoryReferences = new ArrayList<>();
    masterCategoryReferences.add(this.categoryReference);
    category.setMasterCategoryReferences(masterCategoryReferences);
    CategoryAttribute oldCategoryAttribute = new CategoryAttribute();
    oldCategoryAttribute.setId(CategoryServiceTest.CATEGORY_ID);
    CategoryAttribute newCategoryAttribute = new CategoryAttribute();
    newCategoryAttribute.setId(CategoryServiceTest.CATEGORY_ID2);

    List<CategoryAttribute> listOldCategoryAttributes = new ArrayList<>();
    listOldCategoryAttributes.add(oldCategoryAttribute);
    category.setCategoryAttributes(listOldCategoryAttributes);
    Category updatedCategory = category;
    updatedCategory.setDocumentType(DOCUMENTS);

    Mockito.when(this.categoryRepository.findById(CategoryServiceTest.CATEGORY_ID2)).thenReturn(Optional.of(category));
    Mockito.doNothing().when(applicationCacheServiceBean)
        .evictChildCategoryCacheByParentCategoryId(Constants.DEFAULT_STORE_ID, PARENT_CATEGORY_ID);
    Mockito.when(applicationContext.getBean(CategoryService.class)).thenReturn(categoryService);

    this.categoryServiceBean.saveUpdatedCategory(updatedCategory, null, Arrays.asList(PARENT_CATEGORY_ID),
        Arrays.asList(CategoryChangeEventType.DATA_CHANGE));

    verify(this.categoryRepository).findById(CategoryServiceTest.CATEGORY_ID2);
    verify(this.categoryRepository).saveAndFlush(category);
    verify(categoryRepository).findByStoreIdAndParentCategoryIdAndMarkForDeleteFalse(STORE_ID, CATEGORY_ID2);
    verify(this.domainEventPublisherService)
        .publishCategory(categoryArgumentCaptor.capture(), eq(Arrays.asList(CategoryChangeEventType.DATA_CHANGE)),
            Mockito.anySet(), eq(false));
    verify(applicationCacheServiceBean)
        .evictChildCategoryCacheByParentCategoryId(Constants.DEFAULT_STORE_ID, PARENT_CATEGORY_ID);
    assertTrue(DOCUMENTS.equals(categoryArgumentCaptor.getValue().getDocumentType()));
  }

  @Test
  public void saveUpdatedCategoryWithEmptyMasterAndSalesCategoryIdsInfo() throws Exception {
    Category category = getDefaultCategory(2);
    category.setId(CategoryServiceTest.CATEGORY_ID2);
    List<CategoryReference> masterCategoryReferences = new ArrayList<>();
    masterCategoryReferences.add(this.categoryReference);
    category.setMasterCategoryReferences(masterCategoryReferences);
    CategoryAttribute oldCategoryAttribute = new CategoryAttribute();
    oldCategoryAttribute.setId(CategoryServiceTest.CATEGORY_ID);
    CategoryAttribute newCategoryAttribute = new CategoryAttribute();
    newCategoryAttribute.setId(CategoryServiceTest.CATEGORY_ID2);

    List<CategoryAttribute> listOldCategoryAttributes = new ArrayList<>();
    listOldCategoryAttributes.add(oldCategoryAttribute);
    category.setCategoryAttributes(listOldCategoryAttributes);
    Category updatedCategory = category;
    updatedCategory.setDocumentType(DOCUMENTS);

    Mockito.when(this.categoryRepository.findById(CategoryServiceTest.CATEGORY_ID2)).thenReturn(Optional.of(category));
    Mockito.doNothing().when(applicationCacheServiceBean)
        .evictChildCategoryCacheByParentCategoryId(Constants.DEFAULT_STORE_ID, PARENT_CATEGORY_ID);
    Mockito.when(applicationContext.getBean(CategoryService.class)).thenReturn(categoryService);

    this.categoryServiceBean.saveUpdatedCategoryInfo(updatedCategory, null, Arrays.asList(PARENT_CATEGORY_ID),
        Arrays.asList(CategoryChangeEventType.DATA_CHANGE), new HashSet<>(), new ArrayList<>(), new ArrayList<>());

    verify(this.categoryRepository).findById(CategoryServiceTest.CATEGORY_ID2);
    verify(this.categoryRepository).saveAndFlush(category);
    verify(categoryRepository).findByStoreIdAndParentCategoryIdAndMarkForDeleteFalse(STORE_ID, CATEGORY_ID2);
    verify(this.domainEventPublisherService)
        .publishCategory(categoryArgumentCaptor.capture(), eq(Arrays.asList(CategoryChangeEventType.DATA_CHANGE)),
            Mockito.anySet(), eq(false));
    verify(applicationCacheServiceBean)
        .evictChildCategoryCacheByParentCategoryId(Constants.DEFAULT_STORE_ID, PARENT_CATEGORY_ID);
    assertTrue(DOCUMENTS.equals(categoryArgumentCaptor.getValue().getDocumentType()));
  }

  @Test
  public void saveUpdatedCategoryInfo() throws Exception {
    Category category = getDefaultCategory(2);
    category.setId(CategoryServiceTest.CATEGORY_ID2);
    List<CategoryReference> masterCategoryReferences = new ArrayList<>();
    masterCategoryReferences.add(this.categoryReference);
    category.setMasterCategoryReferences(masterCategoryReferences);
    CategoryAttribute oldCategoryAttribute = new CategoryAttribute();
    oldCategoryAttribute.setId(CategoryServiceTest.CATEGORY_ID);
    CategoryAttribute newCategoryAttribute = new CategoryAttribute();
    newCategoryAttribute.setId(CategoryServiceTest.CATEGORY_ID2);

    List<CategoryAttribute> listOldCategoryAttributes = new ArrayList<>();
    listOldCategoryAttributes.add(oldCategoryAttribute);
    category.setCategoryAttributes(listOldCategoryAttributes);
    Category updatedCategory = category;
    updatedCategory.setDocumentType(DOCUMENTS);

    Mockito.when(this.categoryRepository.findById(CategoryServiceTest.CATEGORY_ID2)).thenReturn(Optional.of(category));
    Mockito.doNothing().when(applicationCacheServiceBean)
        .evictChildCategoryCacheByParentCategoryId(Constants.DEFAULT_STORE_ID, PARENT_CATEGORY_ID);
    Mockito.when(applicationContext.getBean(CategoryService.class)).thenReturn(categoryService);

    this.categoryServiceBean.saveUpdatedCategoryInfo(updatedCategory, null, Arrays.asList(PARENT_CATEGORY_ID),
        Arrays.asList(CategoryChangeEventType.DATA_CHANGE), new HashSet<>(), new ArrayList<>(categoryIds), new ArrayList<>(categoryIds));

    verify(this.categoryRepository).findById(CategoryServiceTest.CATEGORY_ID2);
    verify(this.categoryRepository).saveAndFlush(category);
    verify(categoryRepository).findByStoreIdAndParentCategoryIdAndMarkForDeleteFalse(STORE_ID, CATEGORY_ID2);
    verify(this.domainEventPublisherService)
        .publishCategory(categoryArgumentCaptor.capture(), eq(Arrays.asList(CategoryChangeEventType.DATA_CHANGE)),
            Mockito.anySet(), eq(false));
    verify(applicationCacheServiceBean)
        .evictChildCategoryCacheByParentCategoryId(Constants.DEFAULT_STORE_ID, PARENT_CATEGORY_ID);
    assertTrue(DOCUMENTS.equals(categoryArgumentCaptor.getValue().getDocumentType()));
  }


  @Test
  public void findByCategoryIdsTest() {
    categoryServiceBean.findByCategoryIds(STORE_ID, Arrays.asList(CATALOG_ID, CATEGORY_ID2));
    verify(categoryRepository).findByStoreIdAndIdInAndMarkForDeleteFalse(STORE_ID,
        Arrays.asList(CATALOG_ID, CATEGORY_ID2));
  }

  @Test
  public void findActiveChildCountForParent() {
    Mockito.when(this.categoryRepository
        .countByStoreIdAndParentCategoryAndActivatedTrueAndMarkForDeleteFalse(STORE_ID, category)).thenReturn(COUNT);
    categoryServiceBean.findActiveChildCountForParent(STORE_ID, category);
    verify(categoryRepository).countByStoreIdAndParentCategoryAndActivatedTrueAndMarkForDeleteFalse(STORE_ID, category);
  }

  @Test
  public void findInActiveChildCountForParent() {
    Mockito.when(this.categoryRepository
        .countByStoreIdAndParentCategoryAndActivatedFalseAndMarkForDeleteFalse(STORE_ID, category)).thenReturn(COUNT);
    categoryServiceBean.findInActiveChildCountForParent(STORE_ID, category);
    verify(categoryRepository).countByStoreIdAndParentCategoryAndActivatedFalseAndMarkForDeleteFalse(STORE_ID, category);
  }

  @Test
  public void findOverAllChildCountForParent() {
    Mockito.when(this.categoryRepository
        .countByStoreIdAndParentCategoryAndMarkForDeleteFalse(STORE_ID, category)).thenReturn(COUNT);
    categoryServiceBean.findOverAllChildCountForParent(STORE_ID, category);
    verify(categoryRepository).countByStoreIdAndParentCategoryAndMarkForDeleteFalse(STORE_ID, category);
  }

  @Test
  public void findAllParentCategoriesTest() {
    categoryServiceBean.findAllParentCategories(STORE_ID);
    verify(categoryRepository).findByStoreIdAndParentCategoryAndMarkForDeleteFalse(STORE_ID, null);
  }

  @Test
  public void categoryTreeTest() {
    categoryTreeNodeDTOList.add(c1);
    categoryTreeNodeDTOList.add(c2);
    categoryTreeNodeDTOList.add(c3);
    categoryTreeNodeDTOList.add(c4);
    Mockito.when(this.catalogService.findByCatalogType(STORE_ID, CatalogType.MASTER_CATALOG)).thenReturn(catalogList);
    Mockito.when(this.categoryRepository
        .findByActivatedTrueAndMarkForDeleteFalseAndCatalogIdOrderByCategoryCode(MASTER_CATALOG_ID, PageRequest.of(0, 1000)))
        .thenReturn(new PageImpl<>(categoryTreeNodeDTOList));
    Mockito.when(this.categoryConfigurationRepository.findAllCategoryConfigurationDTO(STORE_ID))
        .thenReturn(Arrays.asList(categoryConfigurationDTO));
    List<CategoryTreeNodeResponse> categoryTreeNodeResponseList = categoryServiceBean.getCategoryTree(STORE_ID);
    Mockito.verify(this.categoryRepository).
        findByActivatedTrueAndMarkForDeleteFalseAndCatalogIdOrderByCategoryCode(MASTER_CATALOG_ID, PageRequest.of(0, 1000));
    Mockito.verify(this.categoryConfigurationRepository).findAllCategoryConfigurationDTO(STORE_ID);
    Mockito.verify(this.catalogService).findByCatalogType(STORE_ID, CatalogType.MASTER_CATALOG);
    assertEquals(2, categoryTreeNodeResponseList.get(0).getChild().size());
    assertEquals(0, categoryTreeNodeResponseList.get(0).getChild().get(0).getChild().size());
    assertEquals(0, categoryTreeNodeResponseList.get(1).getChild().size());
  }

  @Test
  public void getCategoryByStoreIdAndIdTest() {
    Mockito.when(categoryRepository.findByStoreIdAndId(STORE_ID, CATEGORY_ID))
        .thenReturn(category);
    Category response =
        categoryServiceBean.getCategoryByStoreIdAndIdCached(STORE_ID, CATEGORY_ID);
    Mockito.verify(categoryRepository).findByStoreIdAndId(STORE_ID, CATEGORY_ID);
    assertEquals(CATEGORY1_NAME, response.getName());
  }

  @Test
  public void findAllChildForC1CategoryCodesTreeTest() throws Exception {
    Mockito.when(this.categoryRepository.findByStoreIdAndCategoryCodeAndMarkForDeleteFalse(STORE_ID, CATEGORY1_CODE))
        .thenReturn(category1);
    Mockito.when(this.categoryRepository.findByStoreIdAndParentCategoryIdAndMarkForDeleteFalse(STORE_ID, CATEGORY_ID1))
        .thenReturn(Collections.singletonList(category2));
    Mockito.when(this.categoryRepository.findByStoreIdAndParentCategoryIdAndMarkForDeleteFalse(STORE_ID, CATEGORY_ID3))
        .thenReturn(Collections.emptyList());
    List<Category> categories = this.categoryServiceBean.findAllChildForC1CategoryCodesTree(STORE_ID, CATEGORY1_CODE);
    Mockito.verify(this.categoryRepository).findByStoreIdAndCategoryCodeAndMarkForDeleteFalse(STORE_ID, CATEGORY1_CODE);
    Mockito.verify(this.categoryRepository).findByStoreIdAndParentCategoryIdAndMarkForDeleteFalse(STORE_ID, CATEGORY_ID1);
    Mockito.verify(this.categoryRepository).findByStoreIdAndParentCategoryIdAndMarkForDeleteFalse(STORE_ID, CATEGORY_ID3);
    assertEquals(2, categories.size());
    assertEquals(CATEGORY_ID3, categories.get(1).getId());
  }

  @Test
  public void findIdsByStoreIdAndCategoryCodesTest() {
    Mockito.when(this.categoryRepository.findIdsByStoreIdAndCategoryCodes(STORE_ID, Arrays.asList(CATEGORY1_CODE)))
        .thenReturn(Arrays.asList(CATEGORY_ID1));
    List<String> categoryIds =
        this.categoryServiceBean.findIdsByStoreIdAndCategoryCodes(STORE_ID, Arrays.asList(CATEGORY1_CODE));
    Mockito.verify(this.categoryRepository).findIdsByStoreIdAndCategoryCodes(STORE_ID, Arrays.asList(CATEGORY1_CODE));
    assertEquals(1, categoryIds.size());
    assertEquals(CATEGORY_ID1, categoryIds.get(0));
  }


  @Test
  public void findIdsByStoreIdAndCategoryCodesExceptionTest() {
    try {
      this.categoryServiceBean.findIdsByStoreIdAndCategoryCodes(STORE_ID, new ArrayList<>());
    } catch (Exception e) {
      assertTrue(e.getMessage().contains(ErrorMessage.CATEGORY_MUST_NOT_BE_BLANK.getMessage()));
    } finally {
      Mockito.verify(this.categoryRepository, times(0))
          .findIdsByStoreIdAndCategoryCodes(Mockito.anyString(), Mockito.anyList());
    }
  }

  @Test
  public void getCategoriesForGenericTemplateTest() throws Exception {
    catalogList = new ArrayList<>();
    Catalog catalog = new Catalog(MASTER_CATALOG);
    this.catalogList.add(catalog);
    Mockito.when(this.categoryRepository.getAllCategoryByCatalogName(STORE_ID, MASTER_CATALOG))
        .thenReturn(this.categoryTreeDTOList);
    Mockito.when(this.catalogRepository.findByStoreIdAndNameStartingWithAndMarkForDeleteFalse(STORE_ID, MASTER_CATALOG))
        .thenReturn(this.catalogList);
    List<CategoryTreeResponse> categoryTreeResponseList =
        this.categoryServiceBean.getAllCategoryTreeforGenericTemplate(STORE_ID, true, true);
    verify(this.catalogRepository).findByStoreIdAndNameStartingWithAndMarkForDeleteFalse(STORE_ID, MASTER_CATALOG);
    verify(this.categoryRepository).getAllCategoryByCatalogName(STORE_ID, MASTER_CATALOG);
    assertEquals("A-01", categoryTreeResponseList.get(0).getCategoryCode());
    assertEquals(StringUtils.EMPTY, categoryTreeResponseList.get(0).getParentCategory());
    assertNull(categoryTreeResponseList.get(1).getChildren());
  }

  @Test
  public void findByStoreIdAndIdInitAllCategoryAttribute() throws Exception {
    Category category = new Category(CategoryServiceTest.STORE_ID, CategoryServiceTest.CATEGORY1_NAME, 1);
    category.setId(CategoryServiceTest.CATEGORY_ID);
    category.setCategoryCode(CategoryServiceTest.CATEGORY1_CODE);
    CategoryAttribute categoryAttribute = new CategoryAttribute();
    categoryAttribute.setId(ID);
    categoryAttribute.setMainDefiningAttribute(true);
    categoryAttribute.setMarkForDelete(true);
    category.setCategoryAttributes(Arrays.asList(categoryAttribute));
    Mockito.when(this.categoryRepository
        .findByStoreIdAndIdAndMarkForDeleteFalse(CategoryServiceTest.STORE_ID, CategoryServiceTest.CATEGORY_ID))
        .thenReturn(category);
    Category response = this.categoryServiceBean
        .findByStoreIdAndIdInitAllCategoryAttribute(CategoryServiceTest.STORE_ID, CategoryServiceTest.CATEGORY_ID);
    assertEquals(CATEGORY_ID, category.getId());
    assertEquals(CATEGORY1_CODE, category.getCategoryCode());
    assertEquals(1, category.getCategoryAttributes().size());
    assertTrue(category.getCategoryAttributes().get(0).isMarkForDelete());
    assertEquals(ID, category.getCategoryAttributes().get(0).getId());
    verify(this.categoryRepository)
        .findByStoreIdAndIdAndMarkForDeleteFalse(CategoryServiceTest.STORE_ID, CategoryServiceTest.CATEGORY_ID);
  }

  @Test
  public void findByStoreIdAndIdInitAllCategoryAttribute_Exception() throws Exception {
    Category category = new Category(CategoryServiceTest.STORE_ID, CategoryServiceTest.CATEGORY1_NAME, 1);
    category.setId(CategoryServiceTest.CATEGORY_ID);
    category.setCategoryCode(CategoryServiceTest.CATEGORY1_CODE);
    CategoryAttribute categoryAttribute = new CategoryAttribute();
    categoryAttribute.setId(ID);
    categoryAttribute.setMainDefiningAttribute(true);
    categoryAttribute.setMarkForDelete(true);
    category.setCategoryAttributes(Arrays.asList(categoryAttribute));
    Mockito.when(this.categoryRepository
        .findByStoreIdAndIdAndMarkForDeleteFalse(CategoryServiceTest.STORE_ID, CategoryServiceTest.CATEGORY_ID))
        .thenReturn(null);
    try {
      this.categoryServiceBean
          .findByStoreIdAndIdInitAllCategoryAttribute(CategoryServiceTest.STORE_ID, CategoryServiceTest.CATEGORY_ID);
    } catch (Exception e) {
      verify(this.categoryRepository)
          .findByStoreIdAndIdAndMarkForDeleteFalse(CategoryServiceTest.STORE_ID, CategoryServiceTest.CATEGORY_ID);
    }
  }

  @Test
  public void getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse() throws Exception{
    when(categoryRepository.findByStoreIdAndCategoryCodeAndMarkForDeleteFalse(STORE_ID, CATEGORY_CODE))
        .thenReturn(category);
    Category category = this.categoryServiceBean.getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(STORE_ID, CATEGORY_CODE);
    verify(categoryRepository).findByStoreIdAndCategoryCodeAndMarkForDeleteFalse(STORE_ID, CATEGORY_CODE);
  }

  @Test
  public void testSaveAndUpdateProductCategoryWithDocumentType() throws Exception {
    this.category.setCategoryCode(CATEGORY_CODE);
    category.setDocumentType(DOCUMENTS);
    Mockito.when(this.categoryRepository.saveAndFlush(this.category)).thenReturn(this.category);

    this.categoryServiceBean.saveAndUpdateProductCategory(CategoryServiceTest.STORE_ID, this.category);

    verify(this.categoryRepository, times(1)).saveAndFlush(categoryArgumentCaptor.capture());
    assertTrue(DOCUMENTS.equals(categoryArgumentCaptor.getValue().getDocumentType()));
  }

  @Test
  public void saveOSCTest() throws Exception {
    categoryServiceBean.saveOSC(new OriginalSalesCategoryRequest());
    verify(originalSalesCategoryService).save(any(OriginalSalesCategory.class));
  }

  @Test
  public void saveOSC_withStoreIdTest() throws Exception {
    OriginalSalesCategoryRequest originalSalesCategoryRequest = new OriginalSalesCategoryRequest();
    originalSalesCategoryRequest.setStoreId(STORE_ID);
    categoryServiceBean.saveOSC(originalSalesCategoryRequest);
    verify(originalSalesCategoryService).save(any(OriginalSalesCategory.class));
  }

  @Test
  public void evictChildCategoryCache_emptyInputTest() throws Exception {
    categoryServiceBean.evictChildCategoryCache(Collections.EMPTY_LIST);
  }

  @Test
  public void saveUpdatedCategory_sameInternalActivation() throws Exception {
    Category category = getDefaultCategory(2);
    category.setId(CategoryServiceTest.CATEGORY_ID2);
    List<CategoryReference> masterCategoryReferences = new ArrayList<>();
    masterCategoryReferences.add(this.categoryReference);
    category.setMasterCategoryReferences(masterCategoryReferences);
    CategoryAttribute oldCategoryAttribute = new CategoryAttribute();
    oldCategoryAttribute.setId(CategoryServiceTest.CATEGORY_ID);
    CategoryAttribute newCategoryAttribute = new CategoryAttribute();
    newCategoryAttribute.setId(CategoryServiceTest.CATEGORY_ID2);

    List<CategoryAttribute> listOldCategoryAttributes = new ArrayList<>();
    listOldCategoryAttributes.add(oldCategoryAttribute);
    category.setCategoryAttributes(listOldCategoryAttributes);
    Category updatedCategory = category;
    updatedCategory.setDocumentType(DOCUMENTS);
    updatedCategory.setInternalActivationInterval(1);

    Mockito.when(this.categoryRepository.findById(CategoryServiceTest.CATEGORY_ID2)).thenReturn(Optional.of(category));
    Mockito.doNothing().when(applicationCacheServiceBean)
        .evictChildCategoryCacheByParentCategoryId(Constants.DEFAULT_STORE_ID, PARENT_CATEGORY_ID);
    Mockito.when(applicationContext.getBean(CategoryService.class)).thenReturn(categoryService);

    this.categoryServiceBean.saveUpdatedCategory(updatedCategory, 1, Arrays.asList(PARENT_CATEGORY_ID),
        Arrays.asList(CategoryChangeEventType.DATA_CHANGE));

    verify(this.categoryRepository).findById(CategoryServiceTest.CATEGORY_ID2);
    verify(this.categoryRepository).saveAndFlush(category);
    verify(categoryRepository).findByStoreIdAndParentCategoryIdAndMarkForDeleteFalse(STORE_ID, CATEGORY_ID2);
    verify(this.domainEventPublisherService)
        .publishCategory(categoryArgumentCaptor.capture(), eq(Arrays.asList(CategoryChangeEventType.DATA_CHANGE)),
            Mockito.anySet(), eq(false));
    verify(applicationCacheServiceBean)
        .evictChildCategoryCacheByParentCategoryId(Constants.DEFAULT_STORE_ID, PARENT_CATEGORY_ID);
    assertTrue(DOCUMENTS.equals(categoryArgumentCaptor.getValue().getDocumentType()));
  }

  @Test
  public void evictActiveChildCategoryCacheTest() throws Exception {
    doNothing().when(applicationCacheServiceBean)
      .evictActiveChildCategoryCacheByParentCategoryId(STORE_ID, PARENT_CATEGORY_ID);
    categoryServiceBean.evictActiveChildCategoryCache(
      Collections.singletonList(PARENT_CATEGORY_ID));
    verify(applicationCacheServiceBean).evictActiveChildCategoryCacheByParentCategoryId(STORE_ID,
      PARENT_CATEGORY_ID);
  }

  @Test
  public void evictActiveChildCategoryCacheWithEmptyListTest() throws Exception {
    doNothing().when(applicationCacheServiceBean)
      .evictActiveChildCategoryCacheByParentCategoryId(STORE_ID, PARENT_CATEGORY_ID);
    categoryServiceBean.evictActiveChildCategoryCache(
      Collections.emptyList());
  }

  @Test
  public void findNameByStoreIdAndCategoryCodesTest() {
    categoryServiceBean.findNameByStoreIdAndCategoryCodes(STORE_ID, Collections.singletonList(CATEGORY1_CODE));
    Mockito.verify(categoryRepository)
        .findNameByStoreIdAndCategoryCodes(STORE_ID, Collections.singletonList(CATEGORY1_CODE));
  }

  @Test
  public void getCategoryAttributesTest() {
    Mockito.when(categoryAttributeRepository.findByStoreIdAndCategoryId(STORE_ID, CATEGORY_ID)).thenReturn(Arrays.asList(categoryAttribute));
    categoryServiceBean.getCategoryAttributes(STORE_ID, CATEGORY_ID);
    Mockito.verify(categoryAttributeRepository).findByStoreIdAndCategoryId(STORE_ID, CATEGORY_ID);
  }

  @Test
  public void findCategoriesByStoreIdAndCategoryCodesTest() {
    Mockito.when(categoryRepository.findByStoreIdAndCategoryCodeInAndMarkForDeleteFalse(STORE_ID, categoryCodes)).thenReturn(categories);
    categoryServiceBean.findCategoriesByStoreIdAndCategoryCodes(STORE_ID, categoryCodes);
    Mockito.verify(categoryRepository).findByStoreIdAndCategoryCodeInAndMarkForDeleteFalse(STORE_ID, categoryCodes);
  }

  @Test
  public void updateB2bExclusiveForChildCategoriesTest() throws Exception {
    Mockito.when(categoryRepository.findByStoreIdAndCategoryCodeAndMarkForDeleteFalse(eq(STORE_ID), any())).thenReturn(category1);
    Mockito.when(categoryRepository.findByStoreIdAndParentCategoryIdAndMarkForDeleteFalse(STORE_ID, category1.getId())).thenReturn(new ArrayList<>());
    categoryServiceBean.updateB2bExclusiveOrHalalCategoryFlagForChildCategories(STORE_ID, category, true, false);
    Mockito.verify(categoryRepository).findByStoreIdAndCategoryCodeAndMarkForDeleteFalse(eq(STORE_ID), any());
    Mockito.verify(categoryRepository).findByStoreIdAndParentCategoryIdAndMarkForDeleteFalse(STORE_ID, category1.getId());
    Mockito.verify(categoryRepository).save(any());
  }

  @Test
  public void updateB2bExclusiveForChildCategoriesEmptyCategoiesTest() throws Exception {
    Mockito.when(categoryRepository.findByStoreIdAndCategoryCodeAndMarkForDeleteFalse(eq(STORE_ID), any())).thenReturn(category);
    Mockito.when(categoryRepository.findByStoreIdAndParentCategoryIdAndMarkForDeleteFalse(STORE_ID, category.getId())).thenReturn(new ArrayList<>());
    categoryServiceBean.updateB2bExclusiveOrHalalCategoryFlagForChildCategories(STORE_ID, category, true, false);
    Mockito.verify(categoryRepository).findByStoreIdAndCategoryCodeAndMarkForDeleteFalse(eq(STORE_ID), any());
    Mockito.verify(categoryRepository).findByStoreIdAndParentCategoryIdAndMarkForDeleteFalse(STORE_ID, category.getId());
  }

  @Test
  public void updateB2bExclusiveForChildCategoriesExceptionTest() throws Exception {
    Mockito.when(categoryRepository.findByStoreIdAndCategoryCodeAndMarkForDeleteFalse(eq(STORE_ID), any())).thenReturn(category1);
    Mockito.when(categoryRepository.findByStoreIdAndParentCategoryIdAndMarkForDeleteFalse(STORE_ID, category1.getId())).thenReturn(new ArrayList<>());
    Mockito.when(categoryRepository.save(any())).thenThrow(RuntimeException.class);
    categoryServiceBean.updateB2bExclusiveOrHalalCategoryFlagForChildCategories(STORE_ID, category, true, false);
    Mockito.verify(categoryRepository).findByStoreIdAndCategoryCodeAndMarkForDeleteFalse(eq(STORE_ID), any());
    Mockito.verify(categoryRepository).findByStoreIdAndParentCategoryIdAndMarkForDeleteFalse(STORE_ID, category1.getId());
    Mockito.verify(categoryRepository).save(any());
  }

  @Test
  public void findChildForParentByCatalogIdB2bExclusiveFalse() throws Exception {
    List<Category> categories = this.getDefaultCategoriesWithCategoryCode();
    categories.get(0).setGenericTemplateEligible(true);
    categories.get(1).setGenericTemplateEligible(false);
    categoryPage = new PageImpl<>(categories, CategoryServiceTest.DEFAULT_PAGEABLE, categories.size());
    Mockito.when(
            this.categoryRepository.findByStoreIdAndParentCategoryIdAndCatalogIdAndDocumentFilterTypeAndB2bExclusiveAndHalalCategoryAndMarkForDeleteFalse(
                STORE_ID, CATEGORY_ID, CATEGORY1_CHILD1_NAME, ALL, true, true, this.defaultPageable))
        .thenReturn(this.categoryPage);
    Page<Category> response =
        this.categoryServiceBean.findChildForParentByCatalogId(STORE_ID, CATEGORY_ID, CATEGORY1_CHILD1_NAME, ALL, true,
            true, this.defaultPageable);
    verify(
        this.categoryRepository).findByStoreIdAndParentCategoryIdAndCatalogIdAndDocumentFilterTypeAndB2bExclusiveAndHalalCategoryAndMarkForDeleteFalse(
        STORE_ID, CATEGORY_ID, CATEGORY1_CHILD1_NAME, ALL, true, true, this.defaultPageable);
    assertTrue(response.getContent().get(0).isGenericTemplateEligible());
    assertFalse(response.getContent().get(1).isGenericTemplateEligible());
  }

  @Test
  public void findChildForParentByCatalogIdWithDocumentFilterTypeB2bExclusiveFalse() throws Exception {
    List<Category> categories = this.getDefaultCategoriesWithCategoryCode();
    categories.get(0).setGenericTemplateEligible(true);
    categories.get(1).setGenericTemplateEligible(false);
    categoryPage = new PageImpl<>(categories, CategoryServiceTest.DEFAULT_PAGEABLE, categories.size());
    Mockito.when(
            this.categoryRepository.findByStoreIdAndParentCategoryIdAndCatalogIdAndDocumentFilterTypeAndB2bExclusiveAndHalalCategoryAndMarkForDeleteFalse(
                STORE_ID, CATEGORY_ID, CATEGORY1_CHILD1_NAME, DOCUMENT_REQUIRED, true, false, this.defaultPageable))
        .thenReturn(this.categoryPage);
    Page<Category> response =
        this.categoryServiceBean.findChildForParentByCatalogId(STORE_ID, CATEGORY_ID, CATEGORY1_CHILD1_NAME,
            DOCUMENT_REQUIRED, true, false, this.defaultPageable);
    verify(
        this.categoryRepository).findByStoreIdAndParentCategoryIdAndCatalogIdAndDocumentFilterTypeAndB2bExclusiveAndHalalCategoryAndMarkForDeleteFalse(
        STORE_ID, CATEGORY_ID, CATEGORY1_CHILD1_NAME, DOCUMENT_REQUIRED, true, false, this.defaultPageable);
    assertTrue(response.getContent().get(0).isGenericTemplateEligible());
    assertFalse(response.getContent().get(1).isGenericTemplateEligible());
  }

  @Test
  public void findChildForParentByCatalogIdWithNoDocumentFilterTypeB2bExclusiveFalse() throws Exception {
    List<Category> categories = this.getDefaultCategoriesWithCategoryCode();
    categories.get(0).setGenericTemplateEligible(true);
    categories.get(1).setGenericTemplateEligible(false);
    categoryPage = new PageImpl<>(categories, CategoryServiceTest.DEFAULT_PAGEABLE, categories.size());
    Mockito.when(
            this.categoryRepository.findByStoreIdAndParentCategoryIdAndCatalogIdAndDocumentFilterTypeAndB2bExclusiveAndHalalCategoryAndMarkForDeleteFalse(
                STORE_ID, CATEGORY_ID, CATEGORY1_CHILD1_NAME, DOCUMENT_NOT_REQUIRED, true, false, this.defaultPageable))
        .thenReturn(this.categoryPage);
    Page<Category> response =
        this.categoryServiceBean.findChildForParentByCatalogId(STORE_ID, CATEGORY_ID, CATEGORY1_CHILD1_NAME,
            DOCUMENT_NOT_REQUIRED, true, false, this.defaultPageable);
    verify(
        this.categoryRepository).findByStoreIdAndParentCategoryIdAndCatalogIdAndDocumentFilterTypeAndB2bExclusiveAndHalalCategoryAndMarkForDeleteFalse(
        STORE_ID, CATEGORY_ID, CATEGORY1_CHILD1_NAME, DOCUMENT_NOT_REQUIRED, true, false, this.defaultPageable);
    assertTrue(response.getContent().get(0).isGenericTemplateEligible());
    assertFalse(response.getContent().get(1).isGenericTemplateEligible());
  }

  @Test
  public void getCategoriesForGenericTemplateBfbExclusiveTest() throws Exception {
    catalogList = new ArrayList<>();
    Catalog catalog = new Catalog(MASTER_CATALOG);
    this.catalogList.add(catalog);
    Mockito.when(this.categoryRepository.getAllCategoryByCatalogName(STORE_ID, MASTER_CATALOG))
      .thenReturn(this.categoryTreeDTOList);
    Mockito.when(this.catalogRepository.findByStoreIdAndNameStartingWithAndMarkForDeleteFalse(STORE_ID, MASTER_CATALOG))
      .thenReturn(this.catalogList);
    List<CategoryTreeResponse> categoryTreeResponseList =
      this.categoryServiceBean.getAllCategoryTreeforGenericTemplate(STORE_ID, true, false);
    verify(this.catalogRepository).findByStoreIdAndNameStartingWithAndMarkForDeleteFalse(STORE_ID, MASTER_CATALOG);
    verify(this.categoryRepository).getAllCategoryByCatalogName(STORE_ID, MASTER_CATALOG);
    assertEquals("A-01", categoryTreeResponseList.get(0).getCategoryCode());
    assertEquals(StringUtils.EMPTY, categoryTreeResponseList.get(0).getParentCategory());
    assertEquals("B-02-01", categoryTreeResponseList.get(1).getCategoryCode());
    assertNull(categoryTreeResponseList.get(1).getChildren());
  }

  @Test
  public void getCategoryAttributesByStoreIdAndCategoryCodesTest() {
    Category category = new Category();
    CategoryAttribute categoryAttribute = new CategoryAttribute();
    Attribute attribute = new Attribute();
    attribute.setAttributeCode(ATTRIBUTE_CODE);
    categoryAttribute.setAttribute(attribute);
    category.setCategoryCode(CATEGORY_CODE);
    category.setCategoryAttributes(Arrays.asList(categoryAttribute));
    Mockito.when(categoryRepository.findByStoreIdAndCategoryCodeInAndMarkForDeleteFalse(STORE_ID,
        Arrays.asList(CATEGORY_CODE))).thenReturn(Arrays.asList(category));
    categoryServiceBean.getCategoryAttributesByStoreIdAndCategoryCodes(STORE_ID,
        Arrays.asList(CATEGORY_CODE));
        verify(categoryRepository).findByStoreIdAndCategoryCodeInAndMarkForDeleteFalse(STORE_ID,
            Arrays.asList(CATEGORY_CODE));
  }


  @Test
  public void getCategoryAttributesByStoreIdAndCategoryCodesTestWithEmptyCategories() {
    Mockito.when(categoryRepository.findByStoreIdAndCategoryCodeInAndMarkForDeleteFalse(STORE_ID,
        Arrays.asList(CATEGORY_CODE))).thenReturn(new ArrayList<>());
    categoryServiceBean.getCategoryAttributesByStoreIdAndCategoryCodes(STORE_ID,
        Arrays.asList(CATEGORY_CODE));
    verify(categoryRepository).findByStoreIdAndCategoryCodeInAndMarkForDeleteFalse(STORE_ID,
        Arrays.asList(CATEGORY_CODE));
  }

  @Test
  public void findCategoryCodesByAttributeCodeTest() {
    List<String> categoryCodes = Arrays.asList(CATEGORY_CODE, CATEGORY_CODE_2);
    Mockito.when(categoryRepository.getCategoryCodesByAttributeCode(STORE_ID, ATTRIBUTE_CODE))
        .thenReturn(categoryCodes);
    List<String> stringList =
        categoryServiceBean.findCategoryCodesByAttributeCode(STORE_ID, ATTRIBUTE_CODE);
    Assertions.assertFalse(stringList.isEmpty());
    Mockito.verify(categoryRepository).getCategoryCodesByAttributeCode(STORE_ID, ATTRIBUTE_CODE);
  }

  @Test
  public void getCategoryAttributesMarkForDeleteFalseTest(){
    List<CategoryAttribute> categoryAttributeList = List.of(new CategoryAttribute());
    Mockito.when(categoryAttributeRepository.findByStoreIdAndCategoryIdAndMarkForDeleteFalse(STORE_ID, CATEGORY_ID)).thenReturn(categoryAttributeList);
    categoryServiceBean.getCategoryAttributesMarkForDeleteFalse(STORE_ID, CATEGORY_ID);
    Mockito.verify(categoryAttributeRepository).findByStoreIdAndCategoryIdAndMarkForDeleteFalse(STORE_ID, CATEGORY_ID);
  }
}
