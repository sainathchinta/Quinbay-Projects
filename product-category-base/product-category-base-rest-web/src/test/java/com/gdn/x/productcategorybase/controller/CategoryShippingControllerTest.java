package com.gdn.x.productcategorybase.controller;

import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.initMocks;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.setup.MockMvcBuilders.standaloneSetup;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.io.FileUtils;
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
import org.springframework.test.web.servlet.MockMvc;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.request.SimpleRequestHolder;
import com.gdn.x.productcategorybase.CategoryShippingApiPath;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.controller.util.MapperUtil;
import com.gdn.x.productcategorybase.dto.request.CategoryShippingRequest;
import com.gdn.x.productcategorybase.dto.request.ShippingRequest;
import com.gdn.x.productcategorybase.entity.CategoryShipping;
import com.gdn.x.productcategorybase.service.CategoryShippingService;

public class CategoryShippingControllerTest {

  private static final String CATEGORY_CODE = "category-code";
  private static final String SHIPPING_CODE = "shipping-code";
  private static final String STORE_ID = "10001";
  private static final String STORE_ID2 = "store-id";
  private static final String CHANNEL_ID = "channel-id";
  private static final String CLIENT_ID = "client-id";
  private static final String REQUEST_ID = "request-id";
  private static final String DEFAULT_USERNAME = "developer";
  private final Integer PAGE = 0;
  private final Integer SIZE = 10;
  private final Pageable pageable = PageRequest.of(this.PAGE, this.SIZE);
  private final ShippingRequest shippingCodeReq = new ShippingRequest(true, true, true);
  private static final double LENGTH = 10;
  private static final double WIDTH = 10;
  private static final double HEIGHT = 10;
  private static final double WEIGHT = 10;

  @InjectMocks
  private CategoryShippingController categoryShippingController;
  @Mock
  private CategoryShippingService categoryShippingService;
  @Mock
  private MapperUtil mapperUtil;
  @Mock
  private Page<CategoryShipping> page;
  @Mock
  private Page<CategoryShipping> pageNull;
  private MockMvc mockMvc;
  private String jsonDeleteReq;
  private SimpleRequestHolder jsonDeleteReqObject;
  private String jsonSaveReq;
  private CategoryShippingRequest jsonSaveReqObject;
  private String jsonSaveNullReq;
  private CategoryShippingRequest jsonSaveNullReqObject;
  private String jsonSaveEmptyCreatedByReq;
  private CategoryShippingRequest jsonSaveEmptyCreatedByReqObject;
  private String jsonSaveEmptyCreatedDateReq;
  private CategoryShippingRequest jsonSaveEmptyCreatedDateReqObject;
  private String jsonUpdateReq;
  private CategoryShippingRequest jsonUpdateReqObject;
  private String jsonUpdateNullReq;
  private CategoryShippingRequest jsonUpdateNullReqObject;
  private String jsonUpdateEmptyUpdatedByReq;
  private CategoryShippingRequest jsonUpdateEmptyUpdatedByReqObject;
  private String jsonUpdateEmptyUpdatedDateReq;
  private CategoryShippingRequest jsonUpdateEmptyUpdatedDateReqObject;

  private CategoryShipping categoryShipping;
  private CategoryShipping categoryShipping2;

  @Test
  public void deleteCategoryShippingTest() throws Exception {
    this.mockMvc.perform(post(CategoryShippingApiPath.BASE_PATH + CategoryShippingApiPath.DELETE_VALUE)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).content(this.jsonDeleteReq)
        .param("storeId", CategoryShippingControllerTest.STORE_ID)
        .param("channelId", CategoryShippingControllerTest.CHANNEL_ID)
        .param("clientId", CategoryShippingControllerTest.CLIENT_ID)
        .param("requestId", CategoryShippingControllerTest.REQUEST_ID)
        .param("username", CategoryShippingControllerTest.DEFAULT_USERNAME));
    verify(this.categoryShippingService, Mockito.times(1))
        .markForDeleteCategoryShipping(CategoryShippingControllerTest.STORE_ID, this.jsonDeleteReqObject.getId());
  }

  @Test
  public void getCategoryShippingByCategoryCodeAndPageableTest() throws Exception {
    this.mockMvc
        .perform(get(CategoryShippingApiPath.BASE_PATH + CategoryShippingApiPath.FILTER_CATEGORY_CODE)
            .param("storeId", CategoryShippingControllerTest.STORE_ID)
            .param("channelId", CategoryShippingControllerTest.CHANNEL_ID)
            .param("clientId", CategoryShippingControllerTest.CLIENT_ID)
            .param("requestId", CategoryShippingControllerTest.REQUEST_ID)
            .param("username", CategoryShippingControllerTest.DEFAULT_USERNAME)
            .param("categoryName", CategoryShippingControllerTest.CATEGORY_CODE))
        .andExpect(jsonPath("$.success", equalTo(true)));

    verify(this.categoryShippingService, Mockito.times(1)).findByCategoryCode(CategoryShippingControllerTest.STORE_ID,
        CategoryShippingControllerTest.CATEGORY_CODE, this.pageable);
    verify(this.page).getContent();
    verify(this.page).getTotalElements();
  }

  @Test
  public void getCategoryShippingByShippingCodeAndPageableTest() throws Exception {
    List<CategoryShipping> test = new ArrayList<CategoryShipping>();
    test.add(this.categoryShipping);
    when(this.mapperUtil.mapRequestToString(this.shippingCodeReq))
        .thenReturn(CategoryShippingControllerTest.SHIPPING_CODE);
    when(this.categoryShippingService.findByShippingCode(CategoryShippingControllerTest.STORE_ID,
        CategoryShippingControllerTest.SHIPPING_CODE, this.pageable)).thenReturn(this.page);
    when(this.page.getContent()).thenReturn(test);
    this.mockMvc
        .perform(get(CategoryShippingApiPath.BASE_PATH + CategoryShippingApiPath.FILTER_SHIPPING_CODE)
            .param("storeId", CategoryShippingControllerTest.STORE_ID)
            .param("channelId", CategoryShippingControllerTest.CHANNEL_ID)
            .param("clientId", CategoryShippingControllerTest.CLIENT_ID)
            .param("requestId", CategoryShippingControllerTest.REQUEST_ID)
            .param("username", CategoryShippingControllerTest.DEFAULT_USERNAME)
            .param("deliveredByMerchant", "" + this.shippingCodeReq.isDeliveredByMerchant())
            .param("specialHandling", "" + this.shippingCodeReq.isSpecialHandling())
            .param("directFlight", "" + this.shippingCodeReq.isDirectFlight()))
        .andExpect(jsonPath("$.success", equalTo(true)));

    verify(this.mapperUtil, Mockito.times(1)).mapRequestToString(this.shippingCodeReq);
    verify(this.categoryShippingService, Mockito.times(1)).findByShippingCode(CategoryShippingControllerTest.STORE_ID,
        CategoryShippingControllerTest.SHIPPING_CODE, this.pageable);
    verify(this.mapperUtil, Mockito.times(1)).mapStringToShippingCodeResponse(this.categoryShipping.getShippingCode());

    verify(this.page).getContent();
    verify(this.page).getTotalElements();
  }

  @Test
  public void getCategoryShippingByShippingCodeSummaryNullTest() throws Exception {
    when(this.categoryShippingService.findByStoreId(CategoryShippingControllerTest.STORE_ID2, this.pageable))
        .thenReturn(this.pageNull);
    when(this.pageNull.getContent()).thenReturn(new ArrayList<CategoryShipping>());
    this.mockMvc
        .perform(get(CategoryShippingApiPath.BASE_PATH).param("storeId", CategoryShippingControllerTest.STORE_ID2)
            .param("channelId", CategoryShippingControllerTest.CHANNEL_ID)
            .param("clientId", CategoryShippingControllerTest.CLIENT_ID)
            .param("requestId", CategoryShippingControllerTest.REQUEST_ID)
            .param("username", CategoryShippingControllerTest.DEFAULT_USERNAME));
    verify(this.categoryShippingService, Mockito.times(1)).findByStoreId(CategoryShippingControllerTest.STORE_ID2,
        this.pageable);

    verify(this.pageNull).getContent();
    verify(this.pageNull).getTotalElements();
  }

  @Test
  public void getCategoryShippingByShippingCodeSummaryTest() throws Exception {
    when(this.categoryShippingService.findByStoreId(CategoryShippingControllerTest.STORE_ID, this.pageable))
        .thenReturn(this.page);
    this.mockMvc
        .perform(get(CategoryShippingApiPath.BASE_PATH).param("storeId", CategoryShippingControllerTest.STORE_ID)
            .param("channelId", CategoryShippingControllerTest.CHANNEL_ID)
            .param("clientId", CategoryShippingControllerTest.CLIENT_ID)
            .param("requestId", CategoryShippingControllerTest.REQUEST_ID)
            .param("username", CategoryShippingControllerTest.DEFAULT_USERNAME));
    verify(this.categoryShippingService, Mockito.times(1)).findByStoreId(CategoryShippingControllerTest.STORE_ID,
        this.pageable);

    verify(this.page).getContent();
    verify(this.page).getTotalElements();
  }

  @Test
  public void saveCategoryShippingTest() throws Exception {
    this.mockMvc.perform(post(CategoryShippingApiPath.BASE_PATH + CategoryShippingApiPath.SAVE)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).content(this.jsonSaveReq)
        .param("storeId", CategoryShippingControllerTest.STORE_ID)
        .param("channelId", CategoryShippingControllerTest.CHANNEL_ID)
        .param("clientId", CategoryShippingControllerTest.CLIENT_ID)
        .param("requestId", CategoryShippingControllerTest.REQUEST_ID)
        .param("username", CategoryShippingControllerTest.DEFAULT_USERNAME));
    verify(this.categoryShippingService, Mockito.times(1)).save(Mockito.any(CategoryShipping.class));
    verify(this.mapperUtil, Mockito.times(1)).mapRequestToString(this.jsonSaveReqObject.getShippingCode());
  }

  @Test
  public void saveCategoryShippingTestEmptyCreatedBy() throws Exception {
    try {
      this.mockMvc.perform(post(CategoryShippingApiPath.BASE_PATH + CategoryShippingApiPath.SAVE)
          .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
          .content(this.jsonSaveEmptyCreatedByReq).param("storeId", CategoryShippingControllerTest.STORE_ID)
          .param("channelId", CategoryShippingControllerTest.CHANNEL_ID)
          .param("clientId", CategoryShippingControllerTest.CLIENT_ID)
          .param("requestId", CategoryShippingControllerTest.REQUEST_ID));
    } catch (Exception e) {
      ApplicationRuntimeException applicationException = (ApplicationRuntimeException) e.getCause();
      Assertions.assertTrue(applicationException.getErrorMessage()
          .contains(ErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_SAVE_MESSAGE.getMessage()));
    }
  }

  @Test
  public void saveCategoryShippingTestEmptyCreatedDate() throws Exception {
    try {
      this.mockMvc.perform(post(CategoryShippingApiPath.BASE_PATH + CategoryShippingApiPath.SAVE)
          .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
          .content(this.jsonSaveEmptyCreatedDateReq).param("storeId", CategoryShippingControllerTest.STORE_ID)
          .param("channelId", CategoryShippingControllerTest.CHANNEL_ID)
          .param("clientId", CategoryShippingControllerTest.CLIENT_ID)
          .param("requestId", CategoryShippingControllerTest.REQUEST_ID));
    } catch (Exception e) {
      ApplicationRuntimeException applicationException = (ApplicationRuntimeException) e.getCause();
      Assertions.assertTrue(applicationException.getErrorMessage()
          .contains(ErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_SAVE_MESSAGE.getMessage()));
    }
  }

  @Test
  public void saveCategoryShippingTestNull() throws Exception {
    this.mockMvc.perform(post(CategoryShippingApiPath.BASE_PATH + CategoryShippingApiPath.SAVE)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).content(this.jsonSaveNullReq)
        .param("storeId", CategoryShippingControllerTest.STORE_ID)
        .param("channelId", CategoryShippingControllerTest.CHANNEL_ID)
        .param("clientId", CategoryShippingControllerTest.CLIENT_ID)
        .param("requestId", CategoryShippingControllerTest.REQUEST_ID)
        .param("username", CategoryShippingControllerTest.DEFAULT_USERNAME));
    verify(this.categoryShippingService, Mockito.times(1)).save(Mockito.any(CategoryShipping.class));
    verify(this.mapperUtil, Mockito.times(1)).mapRequestToString(this.jsonSaveNullReqObject.getShippingCode());
  }

  @BeforeEach
  public void setUp() throws Exception {
    initMocks(this);
    this.mockMvc = standaloneSetup(this.categoryShippingController).build();

    ObjectMapper mapper = new ObjectMapper();

    this.jsonDeleteReq =
        FileUtils.readFileToString(new File("src/test/resources/deleteCategoryShippingCodeRequest.json"));
    this.mockMvc = standaloneSetup(this.categoryShippingController).build();
    this.jsonDeleteReqObject =
        mapper.readValue(this.jsonDeleteReq, mapper.getTypeFactory().constructType(SimpleRequestHolder.class));
    Assertions.assertNotNull(this.jsonDeleteReqObject);

    this.jsonSaveReq = FileUtils.readFileToString(new File("src/test/resources/saveCategoryShippingCodeRequest.json"));
    this.mockMvc = standaloneSetup(this.categoryShippingController).build();
    this.jsonSaveReqObject =
        mapper.readValue(this.jsonSaveReq, mapper.getTypeFactory().constructType(CategoryShippingRequest.class));
    Assertions.assertNotNull(this.jsonSaveReqObject);

    this.jsonSaveNullReq =
        FileUtils.readFileToString(new File("src/test/resources/saveCategoryShippingCodeRequestNull.json"));
    this.mockMvc = standaloneSetup(this.categoryShippingController).build();
    this.jsonSaveNullReqObject =
        mapper.readValue(this.jsonSaveNullReq, mapper.getTypeFactory().constructType(CategoryShippingRequest.class));
    Assertions.assertNotNull(this.jsonSaveNullReqObject);

    this.jsonSaveEmptyCreatedByReq =
        FileUtils.readFileToString(new File("src/test/resources/saveCategoryShippingCodeRequestEmptyCreatedBy.json"));
    this.mockMvc = standaloneSetup(this.categoryShippingController).build();
    this.jsonSaveEmptyCreatedByReqObject = mapper.readValue(this.jsonSaveEmptyCreatedByReq,
        mapper.getTypeFactory().constructType(CategoryShippingRequest.class));
    Assertions.assertNotNull(this.jsonSaveEmptyCreatedByReqObject);

    this.jsonSaveEmptyCreatedDateReq =
        FileUtils.readFileToString(new File("src/test/resources/saveCategoryShippingCodeRequestEmptyCreatedDate.json"));
    this.mockMvc = standaloneSetup(this.categoryShippingController).build();
    this.jsonSaveEmptyCreatedDateReqObject = mapper.readValue(this.jsonSaveEmptyCreatedDateReq,
        mapper.getTypeFactory().constructType(CategoryShippingRequest.class));
    Assertions.assertNotNull(this.jsonSaveEmptyCreatedDateReqObject);

    this.jsonUpdateReq =
        FileUtils.readFileToString(new File("src/test/resources/updateCategoryShippingCodeRequest.json"));
    this.mockMvc = standaloneSetup(this.categoryShippingController).build();
    this.jsonUpdateReqObject =
        mapper.readValue(this.jsonUpdateReq, mapper.getTypeFactory().constructType(CategoryShippingRequest.class));
    Assertions.assertNotNull(this.jsonUpdateReqObject);

    this.jsonUpdateNullReq =
        FileUtils.readFileToString(new File("src/test/resources/updateCategoryShippingCodeRequestNull.json"));
    this.mockMvc = standaloneSetup(this.categoryShippingController).build();
    this.jsonUpdateNullReqObject =
        mapper.readValue(this.jsonUpdateNullReq, mapper.getTypeFactory().constructType(CategoryShippingRequest.class));
    Assertions.assertNotNull(this.jsonUpdateNullReqObject);

    this.jsonUpdateEmptyUpdatedByReq =
        FileUtils.readFileToString(new File("src/test/resources/updateCategoryShippingCodeRequestEmptyUpdatedBy.json"));
    this.mockMvc = standaloneSetup(this.categoryShippingController).build();
    this.jsonUpdateEmptyUpdatedByReqObject = mapper.readValue(this.jsonUpdateEmptyUpdatedByReq,
        mapper.getTypeFactory().constructType(CategoryShippingRequest.class));
    Assertions.assertNotNull(this.jsonUpdateEmptyUpdatedByReqObject);

    this.jsonUpdateEmptyUpdatedDateReq = FileUtils
        .readFileToString(new File("src/test/resources/updateCategoryShippingCodeRequestEmptyUpdatedDate.json"));
    this.mockMvc = standaloneSetup(this.categoryShippingController).build();
    this.jsonUpdateEmptyUpdatedDateReqObject = mapper.readValue(this.jsonUpdateEmptyUpdatedDateReq,
        mapper.getTypeFactory().constructType(CategoryShippingRequest.class));
    Assertions.assertNotNull(this.jsonUpdateEmptyUpdatedDateReqObject);

    this.categoryShipping = new CategoryShipping(CategoryShippingControllerTest.STORE_ID2,
        CategoryShippingControllerTest.CATEGORY_CODE, CategoryShippingControllerTest.SHIPPING_CODE);
    this.categoryShipping2 = new CategoryShipping(CategoryShippingControllerTest.STORE_ID,
        CategoryShippingControllerTest.CATEGORY_CODE, CategoryShippingControllerTest.SHIPPING_CODE);

    when(this.categoryShippingService.findByCategoryCode(CategoryShippingControllerTest.STORE_ID,
        CategoryShippingControllerTest.CATEGORY_CODE, this.pageable)).thenReturn(this.page);
    when(this.categoryShippingService.findByStoreIdAndId(CategoryShippingControllerTest.STORE_ID, "id"))
        .thenReturn(this.categoryShipping2);
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(this.categoryShippingService);
    verifyNoMoreInteractions(this.page);
    verifyNoMoreInteractions(this.pageNull);
    verifyNoMoreInteractions(this.mapperUtil);
  }

  @Test
  public void updateCategoryShippingTest() throws Exception {
    this.mockMvc.perform(post(CategoryShippingApiPath.BASE_PATH + CategoryShippingApiPath.UPDATE)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).content(this.jsonUpdateReq)
        .param("storeId", CategoryShippingControllerTest.STORE_ID)
        .param("channelId", CategoryShippingControllerTest.CHANNEL_ID)
        .param("clientId", CategoryShippingControllerTest.CLIENT_ID)
        .param("requestId", CategoryShippingControllerTest.REQUEST_ID)
        .param("username", CategoryShippingControllerTest.DEFAULT_USERNAME));
    verify(this.categoryShippingService, Mockito.times(1)).findByStoreIdAndId(CategoryShippingControllerTest.STORE_ID,
        "id");
    verify(this.categoryShippingService, Mockito.times(1)).update(this.categoryShipping2);
    verify(this.mapperUtil, Mockito.times(1)).mapRequestToString(this.jsonUpdateReqObject.getShippingCode());
  }

  @Test
  public void updateCategoryShippingTestEmptyUpdatedBy() throws Exception {
    try {
      this.mockMvc.perform(post(CategoryShippingApiPath.BASE_PATH + CategoryShippingApiPath.UPDATE)
          .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
          .content(this.jsonUpdateEmptyUpdatedByReq).param("storeId", CategoryShippingControllerTest.STORE_ID)
          .param("channelId", CategoryShippingControllerTest.CHANNEL_ID)
          .param("clientId", CategoryShippingControllerTest.CLIENT_ID)
          .param("requestId", CategoryShippingControllerTest.REQUEST_ID));
    } catch (Exception e) {
      ApplicationRuntimeException applicationException = (ApplicationRuntimeException) e.getCause();
      Assertions.assertTrue(applicationException.getErrorMessage()
          .contains(ErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_UPDATE_MESSAGE.getMessage()));
    }
  }


  @Test
  public void updateCategoryShippingTestEmptyUpdatedDate() throws Exception {
    try {
      this.mockMvc.perform(post(CategoryShippingApiPath.BASE_PATH + CategoryShippingApiPath.UPDATE)
          .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
          .content(this.jsonUpdateEmptyUpdatedDateReq).param("storeId", CategoryShippingControllerTest.STORE_ID)
          .param("channelId", CategoryShippingControllerTest.CHANNEL_ID)
          .param("clientId", CategoryShippingControllerTest.CLIENT_ID)
          .param("requestId", CategoryShippingControllerTest.REQUEST_ID));
    } catch (Exception e) {
      ApplicationRuntimeException applicationException = (ApplicationRuntimeException) e.getCause();
      Assertions.assertTrue(applicationException.getErrorMessage()
          .contains(ErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_UPDATE_MESSAGE.getMessage()));
    }
  }

  @Test
  public void updateCategoryShippingTestNull() throws Exception {
    this.mockMvc.perform(post(CategoryShippingApiPath.BASE_PATH + CategoryShippingApiPath.UPDATE)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).content(this.jsonUpdateNullReq)
        .param("storeId", CategoryShippingControllerTest.STORE_ID)
        .param("channelId", CategoryShippingControllerTest.CHANNEL_ID)
        .param("clientId", CategoryShippingControllerTest.CLIENT_ID)
        .param("requestId", CategoryShippingControllerTest.REQUEST_ID)
        .param("username", CategoryShippingControllerTest.DEFAULT_USERNAME));
    verify(this.categoryShippingService, Mockito.times(1)).findByStoreIdAndId(CategoryShippingControllerTest.STORE_ID,
        "id");
    verify(this.categoryShippingService, Mockito.times(1)).update(this.categoryShipping2);
    verify(this.mapperUtil, Mockito.times(1)).mapRequestToString(this.jsonUpdateNullReqObject.getShippingCode());
  }

  @Test
  public void updateCategoryShippingTestNullCategoryShipping() throws Exception {
    when(this.categoryShippingService.findByStoreIdAndId(CategoryShippingControllerTest.STORE_ID, "id"))
        .thenReturn(null);
    try {
      this.mockMvc.perform(post(CategoryShippingApiPath.BASE_PATH + CategoryShippingApiPath.UPDATE)
          .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).content(this.jsonUpdateReq)
          .param("storeId", CategoryShippingControllerTest.STORE_ID)
          .param("channelId", CategoryShippingControllerTest.CHANNEL_ID)
          .param("clientId", CategoryShippingControllerTest.CLIENT_ID)
          .param("requestId", CategoryShippingControllerTest.REQUEST_ID));
    } catch (Exception e) {
      verify(this.categoryShippingService, Mockito.times(1)).findByStoreIdAndId(CategoryShippingControllerTest.STORE_ID,
          "id");
      ApplicationRuntimeException applicationException = (ApplicationRuntimeException) e.getCause();
      Assertions.assertTrue(applicationException.getErrorMessage()
          .contains(ErrorMessage.ENTITY_NOT_FOUND_FOR_UPDATE_MESSAGE.getMessage()));
    }
  }

  @Test
  public void generateShippingWeightTest() throws Exception {
    this.mockMvc.perform(
        get(CategoryShippingApiPath.BASE_PATH + CategoryShippingApiPath.GENERATE_SHIPPING_WEIGHT, CATEGORY_CODE)
            .param("storeId", CategoryShippingControllerTest.STORE_ID)
            .param("length", String.valueOf(CategoryShippingControllerTest.LENGTH))
            .param("width", String.valueOf(CategoryShippingControllerTest.WIDTH))
            .param("height", String.valueOf(CategoryShippingControllerTest.HEIGHT))
            .param("weight", String.valueOf(CategoryShippingControllerTest.WEIGHT)))
        .andExpect(jsonPath("$.success", equalTo(true)));

    verify(this.categoryShippingService).generateShippingWeight(CategoryShippingControllerTest.STORE_ID,
        CategoryShippingControllerTest.CATEGORY_CODE, LENGTH, HEIGHT, WEIGHT, WIDTH);
  }

  @Test
  public void generateShippingWeightExceptionTest() throws Exception {
    Mockito.doThrow(new RuntimeException())
        .when(categoryShippingService).generateShippingWeight(STORE_ID, CATEGORY_CODE, LENGTH, HEIGHT, WEIGHT, WIDTH);
    this.mockMvc.perform(
        get(CategoryShippingApiPath.BASE_PATH + CategoryShippingApiPath.GENERATE_SHIPPING_WEIGHT, CATEGORY_CODE)
            .param("storeId", CategoryShippingControllerTest.STORE_ID)
            .param("length", String.valueOf(CategoryShippingControllerTest.LENGTH))
            .param("width", String.valueOf(CategoryShippingControllerTest.WIDTH))
            .param("height", String.valueOf(CategoryShippingControllerTest.HEIGHT))
            .param("weight", String.valueOf(CategoryShippingControllerTest.WEIGHT)))
        .andExpect(jsonPath("$.success", equalTo(false)));

    verify(this.categoryShippingService)
        .generateShippingWeight(CategoryShippingControllerTest.STORE_ID, CategoryShippingControllerTest.CATEGORY_CODE,
            LENGTH, HEIGHT, WEIGHT, WIDTH);
  }

  @Test
  public void generateShippingWeightApplicationExceptionTest() throws Exception {
    Mockito.doThrow(new ApplicationException())
        .when(categoryShippingService).generateShippingWeight(STORE_ID, CATEGORY_CODE, LENGTH, HEIGHT, WEIGHT, WIDTH);
    this.mockMvc.perform(
        get(CategoryShippingApiPath.BASE_PATH + CategoryShippingApiPath.GENERATE_SHIPPING_WEIGHT, CATEGORY_CODE)
            .param("storeId", CategoryShippingControllerTest.STORE_ID)
            .param("length", String.valueOf(CategoryShippingControllerTest.LENGTH))
            .param("width", String.valueOf(CategoryShippingControllerTest.WIDTH))
            .param("height", String.valueOf(CategoryShippingControllerTest.HEIGHT))
            .param("weight", String.valueOf(CategoryShippingControllerTest.WEIGHT)))
        .andExpect(jsonPath("$.success", equalTo(false)));

    verify(this.categoryShippingService)
        .generateShippingWeight(CategoryShippingControllerTest.STORE_ID, CategoryShippingControllerTest.CATEGORY_CODE,
            LENGTH, HEIGHT, WEIGHT, WIDTH);
  }
}
