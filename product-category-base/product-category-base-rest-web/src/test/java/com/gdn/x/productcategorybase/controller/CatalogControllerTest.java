package com.gdn.x.productcategorybase.controller;

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.notNullValue;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.initMocks;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.setup.MockMvcBuilders.standaloneSetup;

import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.UUID;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.http.MediaType;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.supercsv.io.CsvBeanReader;
import org.supercsv.io.CsvBeanWriter;
import org.supercsv.prefs.CsvPreference;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.request.SimpleRequestHolder;
import com.gdn.x.productcategorybase.CatalogType;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.dto.CustomCategoryDto;
import com.gdn.x.productcategorybase.dto.request.CatalogRequest;
import com.gdn.x.productcategorybase.entity.Catalog;
import com.gdn.x.productcategorybase.entity.Category;
import com.gdn.x.productcategorybase.service.CatalogService;
import com.gdn.x.productcategorybase.service.CategoryService;
import com.gdn.x.productcategorybase.util.CommonFactory;

public class CatalogControllerTest {
  private static final ObjectMapper OBJECT_MAPPER = new ObjectMapper();
  private static final String CREATE = "/create";
  private static final String UPDATE = "/update";
  private static final String DELETE = "/delete";
  private static final String ID = "id";
  private static final String PARENT_CATEGORY_ID = "parentcategoryID";
  private static final String CODE = "catalog-code";
  private static final String NAME = "catalog-name";
  private static final String CATEGORIES_BY_CATALOGTYPE = "/filter/categories-by-catalogtype";
  private static final String CLIENT_ID = "client-id";
  private static final String CHANNEL_ID = "channel-id";
  private static final String REQUEST_ID = "REQ-001";
  private static final String DEFAULT_USERNAME = "developer";
  private static final String STORE_ID = "10001";
  private static final String FILTER_NAME = "/filter/name";
  private static final String FILTER_TYPE = "/filter/type";
  private static final int PAGE_SIZE = 10;
  private static final int PAGE_NUMBER = 0;
  private static final int TOTAL_RECORDS = 1;

  @Mock
  private CatalogService service;

  @Mock
  private CategoryService categoryService;

  @Mock
  private Page<Catalog> page;

  @Mock
  private CommonFactory commonFactory;

  @InjectMocks
  private CatalogController controller;

  private MockMvc mockMvc;
  private String jsonReq;
  private CatalogRequest catalogRequest;
  private Catalog catalog;
  private List<Catalog> catalogs;
  private Pageable pageable;
  private CatalogRequest catalogRequestJson;

  @Test
  public void createNewCatalogTest() throws Exception {
    this.mockMvc
        .perform(post(CatalogController.BASE_PATH + CatalogControllerTest.CREATE).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).content(this.jsonReq)
            .param("storeId", CatalogControllerTest.STORE_ID).param("channelId", CatalogControllerTest.CHANNEL_ID)
            .param("clientId", CatalogControllerTest.CLIENT_ID).param("requestId", CatalogControllerTest.REQUEST_ID)
            .param("username", CatalogControllerTest.DEFAULT_USERNAME))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(CatalogControllerTest.REQUEST_ID)));
    verify(this.service).save(Mockito.any(Catalog.class));
  }
  
  @Test
  public void createNewCatalogTestWithEmptyStoreId() throws Exception {
	  this.catalogRequestJson = new CatalogRequest(CatalogControllerTest.NAME, CatalogControllerTest.CODE,
		        "MASTER_CATALOG","");
		    this.catalogRequestJson.setCreatedBy("CREATED-BY");
		    this.catalogRequestJson.setCreatedDate(new Date());
		    this.catalogRequestJson.setUpdatedBy("UPDATED-BY");
		    this.catalogRequestJson.setUpdatedDate(new Date());
    this.mockMvc
        .perform(post(CatalogController.BASE_PATH + CatalogControllerTest.CREATE).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).content(this.jsonReq)
            .content(new ObjectMapper().writeValueAsString(this.catalogRequestJson))
            .param("storeId", CatalogControllerTest.STORE_ID).param("channelId", CatalogControllerTest.CHANNEL_ID)
            .param("clientId", CatalogControllerTest.CLIENT_ID).param("requestId", CatalogControllerTest.REQUEST_ID)
            .param("username", CatalogControllerTest.DEFAULT_USERNAME))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(CatalogControllerTest.REQUEST_ID)));
    verify(this.service).save(Mockito.any(Catalog.class));
  }

  @Test
  public void createNewCatalogWithEmptyCatalogTypeTest() throws Exception {
    this.catalogRequestJson.setCatalogType(null);
    this.jsonReq = CatalogControllerTest.OBJECT_MAPPER.writeValueAsString(this.catalogRequestJson);
    try {
      this.mockMvc
          .perform(post(CatalogController.BASE_PATH + CatalogControllerTest.CREATE).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON).content(this.jsonReq)
              .param("storeId", CatalogControllerTest.STORE_ID).param("channelId", CatalogControllerTest.CHANNEL_ID)
              .param("clientId", CatalogControllerTest.CLIENT_ID).param("requestId", CatalogControllerTest.REQUEST_ID)
              .param("username", CatalogControllerTest.DEFAULT_USERNAME))
          .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)))
          .andExpect(jsonPath("$.requestId", equalTo(CatalogControllerTest.REQUEST_ID)));
    } catch (Exception e) {
      Assertions.assertTrue(e.getCause() instanceof ApplicationRuntimeException);
      ApplicationRuntimeException applicationException = (ApplicationRuntimeException) e.getCause();
      Assertions.assertTrue(applicationException.getErrorMessage()
          .contains(ErrorMessage.ENTITY_REQUIRED_CATALOG_TYPE_FOR_SAVE_MESSAGE.getMessage()));
    }
  }

  @Test
  public void createNewCatalogWithEmptyCreatedByTest() throws Exception {
    this.catalogRequestJson.setCreatedBy(null);
    this.jsonReq = CatalogControllerTest.OBJECT_MAPPER.writeValueAsString(this.catalogRequestJson);
    try {
      this.mockMvc
          .perform(post(CatalogController.BASE_PATH + CatalogControllerTest.CREATE).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON).content(this.jsonReq)
              .param("storeId", CatalogControllerTest.STORE_ID).param("channelId", CatalogControllerTest.CHANNEL_ID)
              .param("clientId", CatalogControllerTest.CLIENT_ID).param("requestId", CatalogControllerTest.REQUEST_ID)
              .param("username", CatalogControllerTest.DEFAULT_USERNAME))
          .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)))
          .andExpect(jsonPath("$.requestId", equalTo(CatalogControllerTest.REQUEST_ID)));
    } catch (Exception e) {
      Assertions.assertTrue(e.getCause() instanceof ApplicationRuntimeException);
      ApplicationRuntimeException applicationException = (ApplicationRuntimeException) e.getCause();
      Assertions.assertTrue(applicationException.getErrorMessage()
          .contains(ErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_SAVE_MESSAGE.getMessage()));
    }
  }

  @Test
  public void createNewCatalogWithEmptyCreatedDateTest() throws Exception {
    this.catalogRequestJson.setCreatedDate(null);
    this.jsonReq = CatalogControllerTest.OBJECT_MAPPER.writeValueAsString(this.catalogRequestJson);
    try {
      this.mockMvc
          .perform(post(CatalogController.BASE_PATH + CatalogControllerTest.CREATE).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON).content(this.jsonReq)
              .param("storeId", CatalogControllerTest.STORE_ID).param("channelId", CatalogControllerTest.CHANNEL_ID)
              .param("clientId", CatalogControllerTest.CLIENT_ID).param("requestId", CatalogControllerTest.REQUEST_ID)
              .param("username", CatalogControllerTest.DEFAULT_USERNAME))
          .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)))
          .andExpect(jsonPath("$.requestId", equalTo(CatalogControllerTest.REQUEST_ID)));
    } catch (Exception e) {
      Assertions.assertTrue(e.getCause() instanceof ApplicationRuntimeException);
      ApplicationRuntimeException applicationException = (ApplicationRuntimeException) e.getCause();
      Assertions.assertTrue(applicationException.getErrorMessage()
          .contains(ErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_SAVE_MESSAGE.getMessage()));
    }
  }

  @Test
  public void deleteCatalogTest() throws Exception {
    SimpleRequestHolder simpleRequestHolder = new SimpleRequestHolder(CatalogControllerTest.ID);
    String request = CatalogControllerTest.OBJECT_MAPPER.writeValueAsString(simpleRequestHolder);
    this.mockMvc
        .perform(post(CatalogController.BASE_PATH + CatalogControllerTest.DELETE).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).content(request).param("storeId", CatalogControllerTest.STORE_ID)
            .param("channelId", CatalogControllerTest.CHANNEL_ID).param("clientId", CatalogControllerTest.CLIENT_ID)
            .param("requestId", CatalogControllerTest.REQUEST_ID)
            .param("username", CatalogControllerTest.DEFAULT_USERNAME))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(CatalogControllerTest.REQUEST_ID)));
    verify(this.service).markForDeleteCatalog(CatalogControllerTest.STORE_ID, CatalogControllerTest.ID);
  }

  @Test
  public void deleteCatalogWithEmptyIdTest() throws Exception {
    SimpleRequestHolder simpleRequestHolder = new SimpleRequestHolder(null);
    String request = CatalogControllerTest.OBJECT_MAPPER.writeValueAsString(simpleRequestHolder);
    try {
      this.mockMvc
          .perform(post(CatalogController.BASE_PATH + CatalogControllerTest.DELETE).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON).content(request).param("storeId", CatalogControllerTest.STORE_ID)
              .param("channelId", CatalogControllerTest.CHANNEL_ID).param("clientId", CatalogControllerTest.CLIENT_ID)
              .param("requestId", CatalogControllerTest.REQUEST_ID)
              .param("username", CatalogControllerTest.DEFAULT_USERNAME))
          .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)))
          .andExpect(jsonPath("$.requestId", equalTo(CatalogControllerTest.REQUEST_ID)));
      verify(this.service).markForDeleteCatalog(CatalogControllerTest.STORE_ID, CatalogControllerTest.ID);
    } catch (Exception e) {
      Assertions.assertTrue(e.getCause() instanceof ApplicationRuntimeException);
      ApplicationRuntimeException applicationException = (ApplicationRuntimeException) e.getCause();
      Assertions.assertTrue(applicationException.getErrorMessage()
          .contains(ErrorMessage.ENTITY_REQUIRED_CATALOG_ID_FOR_DELETE_MESSAGE.getMessage()));
    }
  }

  @Test
  public void getCatalogByNameTest() throws Exception {
    this.mockMvc
        .perform(get(CatalogController.BASE_PATH + CatalogControllerTest.FILTER_NAME)
            .param("storeId", CatalogControllerTest.STORE_ID).param("channelId", CatalogControllerTest.CHANNEL_ID)
            .param("clientId", CatalogControllerTest.CLIENT_ID).param("requestId", CatalogControllerTest.REQUEST_ID)
            .param("username", CatalogControllerTest.DEFAULT_USERNAME).param("name", CatalogControllerTest.NAME))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.pageMetaData.pageSize", equalTo(CatalogControllerTest.PAGE_SIZE)))
        .andExpect(jsonPath("$.pageMetaData.pageNumber", equalTo(CatalogControllerTest.PAGE_NUMBER)))
        .andExpect(jsonPath("$.pageMetaData.totalRecords", equalTo(CatalogControllerTest.TOTAL_RECORDS)))
        .andExpect(jsonPath("$.content[0].name", equalTo(CatalogControllerTest.NAME)))
        .andExpect(jsonPath("$.content[0].catalogType", equalTo(CatalogType.MASTER_CATALOG.toString())));
    verify(this.service).findByName(CatalogControllerTest.STORE_ID, CatalogControllerTest.NAME, this.pageable);
    verify(this.page).getContent();
    verify(this.page).getTotalElements();
  }

  @Test
  public void getCatalogByTypeTest() throws Exception {
    this.mockMvc
        .perform(get(CatalogController.BASE_PATH + CatalogControllerTest.FILTER_TYPE).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).param("storeId", CatalogControllerTest.STORE_ID)
            .param("channelId", CatalogControllerTest.CHANNEL_ID).param("clientId", CatalogControllerTest.CLIENT_ID)
            .param("requestId", CatalogControllerTest.REQUEST_ID)
            .param("username", CatalogControllerTest.DEFAULT_USERNAME)
            .param("catalogType", String.valueOf(CatalogType.MASTER_CATALOG)))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.pageMetaData.pageSize", equalTo(CatalogControllerTest.PAGE_SIZE)))
        .andExpect(jsonPath("$.pageMetaData.pageNumber", equalTo(CatalogControllerTest.PAGE_NUMBER)))
        .andExpect(jsonPath("$.pageMetaData.totalRecords", equalTo(CatalogControllerTest.TOTAL_RECORDS)))
        .andExpect(jsonPath("$.content[0].name", equalTo(CatalogControllerTest.NAME)))
        .andExpect(jsonPath("$.content[0].catalogType", equalTo(CatalogType.MASTER_CATALOG.toString())));
    verify(this.service).findByCatalogType(CatalogControllerTest.STORE_ID, CatalogType.MASTER_CATALOG, this.pageable);
    verify(this.page).getContent();
    verify(this.page).getTotalElements();
  }

  @Test
  public void getCatalogDetailTestWithEmptyParentCategory() throws Exception {
    Catalog catalog = new Catalog(CatalogControllerTest.NAME, CatalogControllerTest.CODE, CatalogType.MASTER_CATALOG,
        CatalogControllerTest.STORE_ID);
    List<Category> categories = new ArrayList<Category>();
    Category category = new Category();
    category.setId("1111");
    categories.add(category);
    catalog.setCategories(categories);
    Mockito.when(this.service.findDetailByStoreIdAndId(CatalogControllerTest.STORE_ID, CatalogControllerTest.ID))
        .thenReturn(catalog);
    this.mockMvc
        .perform(get(CatalogController.BASE_PATH + "/" + CatalogControllerTest.ID).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).param("storeId", CatalogControllerTest.STORE_ID)
            .param("channelId", CatalogControllerTest.CHANNEL_ID).param("clientId", CatalogControllerTest.CLIENT_ID)
            .param("requestId", CatalogControllerTest.REQUEST_ID)
            .param("username", CatalogControllerTest.DEFAULT_USERNAME))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(CatalogControllerTest.REQUEST_ID)))
        .andExpect(jsonPath("$.value.name", equalTo(CatalogControllerTest.NAME)))
        .andExpect(jsonPath("$.value.catalogType", equalTo(CatalogType.MASTER_CATALOG.toString())))
        .andExpect(jsonPath("$.value.categories[0].id", equalTo(category.getId())));
    verify(this.service).findDetailByStoreIdAndId(CatalogControllerTest.STORE_ID, CatalogControllerTest.ID);
  }
  
  @Test
  public void getCatalogDetailTest() throws Exception {
    Catalog catalog = new Catalog(CatalogControllerTest.NAME, CatalogControllerTest.CODE, CatalogType.MASTER_CATALOG,
        CatalogControllerTest.STORE_ID);
    List<Category> categories = new ArrayList<Category>();
    Category category = new Category();
    Category parentCategory = new Category();
    parentCategory.setId("11122");
    category.setId("1111");
    category.setParentCategory(parentCategory);
    categories.add(category);
    catalog.setCategories(categories);
    Mockito.when(this.service.findDetailByStoreIdAndId(CatalogControllerTest.STORE_ID, CatalogControllerTest.ID))
        .thenReturn(catalog);
    this.mockMvc
        .perform(get(CatalogController.BASE_PATH + "/" + CatalogControllerTest.ID).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).param("storeId", CatalogControllerTest.STORE_ID)
            .param("channelId", CatalogControllerTest.CHANNEL_ID).param("clientId", CatalogControllerTest.CLIENT_ID)
            .param("requestId", CatalogControllerTest.REQUEST_ID)
            .param("username", CatalogControllerTest.DEFAULT_USERNAME))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(CatalogControllerTest.REQUEST_ID)))
        .andExpect(jsonPath("$.value.name", equalTo(CatalogControllerTest.NAME)))
        .andExpect(jsonPath("$.value.catalogType", equalTo(CatalogType.MASTER_CATALOG.toString())))
        .andExpect(jsonPath("$.value.categories[0].id", equalTo(category.getId())));
    verify(this.service).findDetailByStoreIdAndId(CatalogControllerTest.STORE_ID, CatalogControllerTest.ID);
  }

  @Test
  public void getMasterWarehouseSummaryTest() throws Exception {
    this.mockMvc
        .perform(
            get(CatalogController.BASE_PATH).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .param("storeId", CatalogControllerTest.STORE_ID).param("channelId", CatalogControllerTest.CHANNEL_ID)
                .param("clientId", CatalogControllerTest.CLIENT_ID).param("requestId", CatalogControllerTest.REQUEST_ID)
                .param("username", CatalogControllerTest.DEFAULT_USERNAME))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(CatalogControllerTest.REQUEST_ID)))
        .andExpect(jsonPath("$.content[0].name", equalTo(CatalogControllerTest.NAME)))
        .andExpect(jsonPath("$.content[0].catalogType", equalTo(CatalogType.MASTER_CATALOG.toString())));
    verify(this.service).findByStoreId(CatalogControllerTest.STORE_ID, this.pageable);
    verify(this.page).getContent();
    verify(this.page).getTotalElements();
  }

  @BeforeEach
  public void setUp() throws Exception {
    initMocks(this);
    this.mockMvc = standaloneSetup(this.controller).build();
    this.catalogRequestJson = new CatalogRequest(CatalogControllerTest.NAME, CatalogControllerTest.CODE,
        "MASTER_CATALOG", CatalogControllerTest.STORE_ID);
    this.catalogRequestJson.setCreatedBy("CREATED-BY");
    this.catalogRequestJson.setCreatedDate(new Date());
    this.catalogRequestJson.setUpdatedBy("UPDATED-BY");
    this.catalogRequestJson.setUpdatedDate(new Date());
    this.jsonReq = CatalogControllerTest.OBJECT_MAPPER.writeValueAsString(this.catalogRequestJson);

    this.catalogRequest = CatalogControllerTest.OBJECT_MAPPER.readValue(this.jsonReq,
        CatalogControllerTest.OBJECT_MAPPER.getTypeFactory().constructType(CatalogRequest.class));
    Assertions.assertNotNull(this.catalogRequest);

    this.catalog = new Catalog();
    this.catalog.setCatalogType(CatalogType.MASTER_CATALOG);
    this.catalog.setName(CatalogControllerTest.NAME);
    this.catalogs = new ArrayList<>();
    this.catalogs.add(this.catalog);
    this.pageable = PageRequest.of(CatalogControllerTest.PAGE_NUMBER, CatalogControllerTest.PAGE_SIZE);

    when(this.service.findByName(CatalogControllerTest.STORE_ID, CatalogControllerTest.NAME, this.pageable))
        .thenReturn(this.page);
    when(this.service.findByCatalogType(CatalogControllerTest.STORE_ID, CatalogType.MASTER_CATALOG, this.pageable))
        .thenReturn(this.page);
    when(this.service.findById(CatalogControllerTest.ID)).thenReturn(this.catalog);
    when(this.service.findByStoreId(CatalogControllerTest.STORE_ID, this.pageable)).thenReturn(this.page);
    when(this.page.getContent()).thenReturn(this.catalogs);
    when(this.page.getTotalElements()).thenReturn((long) CatalogControllerTest.TOTAL_RECORDS);

  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(this.service);
    verifyNoMoreInteractions(this.page);
  }

  @Test
  public void updateCatalogTest() throws Exception {
    this.catalogRequestJson.setId(CatalogControllerTest.ID);
    this.catalog.setId(CatalogControllerTest.ID);
    this.catalog.setStoreId(CatalogControllerTest.ID);
    Mockito.when(this.service.findByStoreIdAndId(CatalogControllerTest.STORE_ID, CatalogControllerTest.ID))
        .thenReturn(this.catalog);
    this.jsonReq = CatalogControllerTest.OBJECT_MAPPER.writeValueAsString(this.catalogRequestJson);
    this.mockMvc
        .perform(post(CatalogController.BASE_PATH + CatalogControllerTest.UPDATE).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).content(this.jsonReq)
            .param("storeId", CatalogControllerTest.STORE_ID).param("channelId", CatalogControllerTest.CHANNEL_ID)
            .param("clientId", CatalogControllerTest.CLIENT_ID).param("requestId", CatalogControllerTest.REQUEST_ID)
            .param("username", CatalogControllerTest.DEFAULT_USERNAME))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(CatalogControllerTest.REQUEST_ID)));
    verify(this.service).update(this.catalog);
    verify(this.service).findByStoreIdAndId(CatalogControllerTest.STORE_ID, CatalogControllerTest.ID);
  }
  
  @Test
  public void updateCatalogTestWithEmptyStoreId() throws Exception {
    this.catalog.setId(CatalogControllerTest.ID);
    this.catalogRequestJson = new CatalogRequest(CatalogControllerTest.NAME, CatalogControllerTest.CODE,
	        "MASTER_CATALOG","");
	    this.catalogRequestJson.setCreatedBy("CREATED-BY");
	    this.catalogRequestJson.setCreatedDate(new Date());
	    this.catalogRequestJson.setUpdatedBy("UPDATED-BY");
	    this.catalogRequestJson.setUpdatedDate(new Date());
	    this.catalogRequestJson.setId(CatalogControllerTest.ID);
    Mockito.when(this.service.findByStoreIdAndId(CatalogControllerTest.STORE_ID, CatalogControllerTest.ID))
        .thenReturn(this.catalog);
    this.mockMvc
        .perform(post(CatalogController.BASE_PATH + CatalogControllerTest.UPDATE).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON)
            .content(new ObjectMapper().writeValueAsString(this.catalogRequestJson))
            .param("storeId", CatalogControllerTest.STORE_ID).param("channelId", CatalogControllerTest.CHANNEL_ID)
            .param("clientId", CatalogControllerTest.CLIENT_ID).param("requestId", CatalogControllerTest.REQUEST_ID)
            .param("username", CatalogControllerTest.DEFAULT_USERNAME))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(CatalogControllerTest.REQUEST_ID)));
    verify(this.service).update(this.catalog);
    verify(this.service).findByStoreIdAndId(CatalogControllerTest.STORE_ID, CatalogControllerTest.ID);
  }

  @Test
  public void updateCatalogWithEmptyIdTest() throws Exception {
    this.jsonReq = CatalogControllerTest.OBJECT_MAPPER.writeValueAsString(this.catalogRequestJson);
    try {
      this.mockMvc
          .perform(post(CatalogController.BASE_PATH + CatalogControllerTest.UPDATE).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON).content(this.jsonReq)
              .param("storeId", CatalogControllerTest.STORE_ID).param("channelId", CatalogControllerTest.CHANNEL_ID)
              .param("clientId", CatalogControllerTest.CLIENT_ID).param("requestId", CatalogControllerTest.REQUEST_ID)
              .param("username", CatalogControllerTest.DEFAULT_USERNAME))
          .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)))
          .andExpect(jsonPath("$.requestId", equalTo(CatalogControllerTest.REQUEST_ID)));
    } catch (Exception e) {
      Assertions.assertTrue(e.getCause() instanceof ApplicationRuntimeException);
      ApplicationRuntimeException applicationException = (ApplicationRuntimeException) e.getCause();
      Assertions.assertTrue(applicationException.getErrorMessage()
          .contains(ErrorMessage.ENTITY_NOT_FOUND_FOR_UPDATE_MESSAGE.getMessage()));
      verify(this.service).findByStoreIdAndId(CatalogControllerTest.STORE_ID, null);
    }
  }

  @Test
  public void updateCatalogWithEmptyUpdatedByTest() throws Exception {
    this.catalogRequestJson.setId(CatalogControllerTest.ID);
    this.catalogRequestJson.setUpdatedBy(null);
    this.jsonReq = CatalogControllerTest.OBJECT_MAPPER.writeValueAsString(this.catalogRequestJson);
    try {
      this.mockMvc
          .perform(post(CatalogController.BASE_PATH + CatalogControllerTest.UPDATE).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON).content(this.jsonReq)
              .param("storeId", CatalogControllerTest.STORE_ID).param("channelId", CatalogControllerTest.CHANNEL_ID)
              .param("clientId", CatalogControllerTest.CLIENT_ID).param("requestId", CatalogControllerTest.REQUEST_ID)
              .param("username", CatalogControllerTest.DEFAULT_USERNAME))
          .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)))
          .andExpect(jsonPath("$.requestId", equalTo(CatalogControllerTest.REQUEST_ID)));
    } catch (Exception e) {
      Assertions.assertTrue(e.getCause() instanceof ApplicationRuntimeException);
      ApplicationRuntimeException applicationException = (ApplicationRuntimeException) e.getCause();
      Assertions.assertTrue(applicationException.getErrorMessage()
          .contains(ErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_UPDATE_MESSAGE.getMessage()));
    }
  }

  @Test
  public void updateCatalogWithEmptyUpdatedDateTest() throws Exception {
    this.catalogRequestJson.setId(CatalogControllerTest.ID);
    this.catalogRequestJson.setUpdatedDate(null);
    this.jsonReq = CatalogControllerTest.OBJECT_MAPPER.writeValueAsString(this.catalogRequestJson);
    try {
      this.mockMvc
          .perform(post(CatalogController.BASE_PATH + CatalogControllerTest.UPDATE).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON).content(this.jsonReq)
              .param("storeId", CatalogControllerTest.STORE_ID).param("channelId", CatalogControllerTest.CHANNEL_ID)
              .param("clientId", CatalogControllerTest.CLIENT_ID).param("requestId", CatalogControllerTest.REQUEST_ID)
              .param("username", CatalogControllerTest.DEFAULT_USERNAME))
          .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)))
          .andExpect(jsonPath("$.requestId", equalTo(CatalogControllerTest.REQUEST_ID)));
    } catch (Exception e) {
      Assertions.assertTrue(e.getCause() instanceof ApplicationRuntimeException);
      ApplicationRuntimeException applicationException = (ApplicationRuntimeException) e.getCause();
      Assertions.assertTrue(applicationException.getErrorMessage()
          .contains(ErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_UPDATE_MESSAGE.getMessage()));
    }
  }

  @Test
  public void uploadCatalogTest() throws Exception {
    String catalogId = UUID.randomUUID().toString();
    Catalog catalog = new Catalog("Catalog1", "Catalog1", CatalogType.MASTER_CATALOG, CatalogControllerTest.STORE_ID);
    Mockito.when(this.service.save(catalog)).thenReturn(catalogId);
    Mockito.when(this.service.findByStoreIdAndId(CatalogControllerTest.STORE_ID, catalogId)).thenReturn(catalog);
    InputStream in = this.getClass().getResourceAsStream("/Catalog.csv");
    MockMultipartFile file = new MockMultipartFile("catalogCsvFile", in);
    CsvBeanReader csvBeanReader = new CsvBeanReader(
            new InputStreamReader(file.getInputStream(),"UTF-8"),CsvPreference.STANDARD_PREFERENCE);
    CsvBeanWriter csvBeanWriter = new CsvBeanWriter(new PrintWriter(new ByteArrayOutputStream(1024)),
            CsvPreference.STANDARD_PREFERENCE);
    Mockito.when(commonFactory.createCsvBeanReader(any(InputStream.class),eq("UTF-8"))).thenReturn(csvBeanReader);
    Mockito.when(commonFactory.createCsvBeanWriter(any(Writer.class))).thenReturn(csvBeanWriter);
    this.mockMvc.perform(MockMvcRequestBuilders.multipart(CatalogController.BASE_PATH + "/upload").file(file)
        .param("storeId", CatalogControllerTest.STORE_ID).param("channelId", CatalogControllerTest.CHANNEL_ID)
        .param("clientId", CatalogControllerTest.CLIENT_ID).param("requestId", CatalogControllerTest.REQUEST_ID)
        .param("username", CatalogControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk());
    Mockito.verify(this.service).save(catalog);
    Mockito.verify(this.service).findByStoreIdAndId(CatalogControllerTest.STORE_ID, catalogId);
  }

  @Test
  public void getCategoriesFromCatalogTypeTest() throws Exception {
    CustomCategoryDto customCategoryEntity =
        new CustomCategoryDto(ID, CODE, NAME, PARENT_CATEGORY_ID, 100);
    List<CustomCategoryDto> customCategoryEntityList = new ArrayList<>();
    customCategoryEntityList.add(customCategoryEntity);
    Mockito.when(
        this.categoryService.getCategoriesFromCatalogType(STORE_ID, CatalogType.MASTER_CATALOG))
        .thenReturn(customCategoryEntityList);
    this.mockMvc
        .perform(get(CatalogController.BASE_PATH + CatalogControllerTest.CATEGORIES_BY_CATALOGTYPE)
            .accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON)
            .param("storeId", CatalogControllerTest.STORE_ID)
            .param("channelId", CatalogControllerTest.CHANNEL_ID)
            .param("clientId", CatalogControllerTest.CLIENT_ID)
            .param("requestId", CatalogControllerTest.REQUEST_ID)
            .param("username", CatalogControllerTest.DEFAULT_USERNAME)
            .param("catalogType", String.valueOf(CatalogType.MASTER_CATALOG)))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.value.catalogType", equalTo(CatalogType.MASTER_CATALOG.toString())))
        .andExpect(jsonPath("$.value.categories[0].id", equalTo(ID)));
    Mockito.verify(this.categoryService)
        .getCategoriesFromCatalogType(STORE_ID, CatalogType.MASTER_CATALOG);
  }
  @Test
  public void getCategoriesFromCatalogType_throwException() throws Exception {
    CustomCategoryDto customCategoryEntity =
            new CustomCategoryDto(ID, CODE, NAME, PARENT_CATEGORY_ID, 100);
    List<CustomCategoryDto> customCategoryEntityList = new ArrayList<>();
    customCategoryEntityList.add(customCategoryEntity);
    doThrow(new Exception()).when(
            this.categoryService).getCategoriesFromCatalogType(STORE_ID, CatalogType.MASTER_CATALOG);
    this.mockMvc
            .perform(get(CatalogController.BASE_PATH + CatalogControllerTest.CATEGORIES_BY_CATALOGTYPE)
                    .accept(MediaType.APPLICATION_JSON)
                    .contentType(MediaType.APPLICATION_JSON)
                    .param("storeId", CatalogControllerTest.STORE_ID)
                    .param("channelId", CatalogControllerTest.CHANNEL_ID)
                    .param("clientId", CatalogControllerTest.CLIENT_ID)
                    .param("requestId", CatalogControllerTest.REQUEST_ID)
                    .param("username", CatalogControllerTest.DEFAULT_USERNAME)
                    .param("catalogType", String.valueOf(CatalogType.MASTER_CATALOG)))
            .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(false)));
    Mockito.verify(this.categoryService)
            .getCategoriesFromCatalogType(STORE_ID, CatalogType.MASTER_CATALOG);
  }
}
