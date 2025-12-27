package com.gdn.x.productcategorybase.controller;

import static com.gdn.x.productcategorybase.SolrConstants.NA;
import static org.mockito.Mockito.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.initMocks;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.setup.MockMvcBuilders.standaloneSetup;

import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

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
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.request.SimpleRequestHolder;
import com.gdn.x.productcategorybase.PredefinedAllowedAttributeValueApiPath;
import com.gdn.x.productcategorybase.dto.request.PredefinedAllowedAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.entity.Attribute;
import com.gdn.x.productcategorybase.entity.PredefinedAllowedAttributeValue;
import com.gdn.x.productcategorybase.service.PredefinedAllowedAttributeValueService;
import com.gdn.x.productcategorybase.service.brand.BrandService;


public class PredefinedAllowedAttributeValueControllerTest {

  private static final String VALUE = "Value";

  private static final ObjectMapper OBJECT_MAPPER = new ObjectMapper();

  private static final String ID = "id";

  private static final String NAME = "name";
  private static final String ATTRIBUTE_CODE = "ATT";

  private static final String STORE_ID = "10001";

  private static final String CLIENT_ID = "client-id";

  private static final String CHANNEL_ID = "channel-id";

  private static final String REQUEST_ID = "REQ-001";
  private static final String DEFAULT_USERNAME = "developer";

  private static final int PAGE_NUMBER = 0;

  private static final int PAGE_SIZE = 10;

  private static final long TOTAL_RECORDS = 0;
  private final Pageable pageable =
      PageRequest.of(PredefinedAllowedAttributeValueControllerTest.PAGE_NUMBER,
          PredefinedAllowedAttributeValueControllerTest.PAGE_SIZE);
  @InjectMocks
  private PredefinedAllowedAttributeValueController controller;
  @Mock
  private PredefinedAllowedAttributeValueService service;
  @Mock
  private BrandService brandServiceBean;
  @Mock
  private Page<PredefinedAllowedAttributeValue> page;
  @Mock
  private Page<PredefinedAllowedAttributeValueResponse> responsePage;
  private MockMvc mockMvc;
  private List<PredefinedAllowedAttributeValue> predefinedAllowedAttributeValueList;
  private List<PredefinedAllowedAttributeValueResponse> responseList;

  private Attribute attribute;
  private Attribute predefineAttribute;
  private Attribute decroptiveAttribute;

  @Test
  public void deactivatedTest() throws Exception {
    SimpleRequestHolder simpleRequestHolder =
        new SimpleRequestHolder(PredefinedAllowedAttributeValueControllerTest.ID);
    String request = PredefinedAllowedAttributeValueControllerTest.OBJECT_MAPPER
        .writeValueAsString(simpleRequestHolder);
    this.mockMvc.perform(get(PredefinedAllowedAttributeValueApiPath.BASE_PATH
        + PredefinedAllowedAttributeValueApiPath.DEACTIVATED).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).content("").param("storeId", STORE_ID)
            .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID).param("username", DEFAULT_USERNAME).param("id", ID))
        .andExpect(status().isOk());
    verify(this.service).deactivated(ID, STORE_ID);
  }
  
  @Test
  public void getPredefinedAllowedAttributeValueByAttributeCodeAndValueTest() throws Exception {
    when(
        this.service
            .findTopByStoreIdAndAttributeCodeAndValueOrPredefinedAllowedAttributeCode(
                STORE_ID, ATTRIBUTE_CODE, VALUE, false)).thenReturn(
        predefinedAllowedAttributeValueList.get(0));
    this.mockMvc.perform(
        get(
            PredefinedAllowedAttributeValueApiPath.BASE_PATH
                + PredefinedAllowedAttributeValueApiPath.GET_BY_ATTRIBUTE_CODE_AND_VALUE)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).content("")
            .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID).param("username", DEFAULT_USERNAME)
            .param("attributeCode", ATTRIBUTE_CODE).param("value", VALUE)).andExpect(
        status().isOk());
    verify(this.service)
        .findTopByStoreIdAndAttributeCodeAndValueOrPredefinedAllowedAttributeCode(
            STORE_ID, ATTRIBUTE_CODE, VALUE, false);
  }
  
  @Test
  public void getPredefinedAllowedAttributeValueByAttributeCodeAndValueNotOkTest() throws Exception {
    when(
        this.service
            .findTopByStoreIdAndAttributeCodeAndValueOrPredefinedAllowedAttributeCode(
                STORE_ID, ATTRIBUTE_CODE, VALUE, false)).thenReturn(null);
    try {
      this.mockMvc.perform(
          get(
              PredefinedAllowedAttributeValueApiPath.BASE_PATH
                  + PredefinedAllowedAttributeValueApiPath.GET_BY_ATTRIBUTE_CODE_AND_VALUE)
              .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
              .content("").param("storeId", STORE_ID).param("channelId", CHANNEL_ID)
              .param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
              .param("username", DEFAULT_USERNAME).param("attributeCode", ATTRIBUTE_CODE)
              .param("value", VALUE)).andExpect(status().isInternalServerError());
    } catch (Exception e) {
      verify(this.service)
          .findTopByStoreIdAndAttributeCodeAndValueOrPredefinedAllowedAttributeCode(
              STORE_ID, ATTRIBUTE_CODE, VALUE, false);
    }
  }

  @Test
  public void getPredefinedAllowedAttributeValueByAttributeIdAndValueAndPageableTest()
      throws Exception {
    when(this.service.findByStoreIdAndAttributeIdAndValueLikeIgnoreCaseAndMarkForDeleteFalse(
        Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.any(Pageable.class))).thenReturn(this.page);
    when(this.page.getContent()).thenReturn(this.predefinedAllowedAttributeValueList);
    when(this.page.getTotalElements()).thenReturn(TOTAL_RECORDS);
    this.mockMvc.perform(get(PredefinedAllowedAttributeValueApiPath.BASE_PATH
        + PredefinedAllowedAttributeValueApiPath.FILTER_ATTRIBUTE_ID_AND_VALUE)
            .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID).param("username", DEFAULT_USERNAME)
            .param("attributeId", ID).param("value", VALUE))
        .andExpect(status().isOk());
    verify(this.service).findByStoreIdAndAttributeIdAndValueLikeIgnoreCaseAndMarkForDeleteFalse(
        Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.any(Pageable.class));
  }

  @Test
  public void getSpecificPredefinedAllowedAttributeValueByAttributeIdAndValueTest() throws Exception {
    when(this.service
        .findByStoreIdAndAttributeIdAndValueAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString())).thenReturn(Arrays.asList(new PredefinedAllowedAttributeValue()));
    this.mockMvc.perform(get(PredefinedAllowedAttributeValueApiPath.BASE_PATH
        + PredefinedAllowedAttributeValueApiPath.GET_ATTRIBUTE_BY_ID_AND_VALUE).param("storeId", STORE_ID)
        .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
        .param("username", DEFAULT_USERNAME).param("attributeId", ID).param("value", VALUE)).andExpect(status().isOk());
    verify(this.service).findByStoreIdAndAttributeIdAndValueAndMarkForDeleteFalse(STORE_ID, VALUE, ID);
  }

  @Test
  public void getBrandSuggestionsTest() throws Exception {
    Pageable pageable = PageRequest.of(0, 10);
    when(this.service
        .getBrandSuggestions(eq(STORE_ID), eq(VALUE), eq(NA), eq(pageable), eq(Boolean.FALSE), eq(Boolean.FALSE)))
        .thenReturn(this.responsePage);
    when(this.responsePage.getContent()).thenReturn(this.responseList);
    when(this.responsePage.getTotalElements()).thenReturn(TOTAL_RECORDS);
    this.mockMvc.perform(get(PredefinedAllowedAttributeValueApiPath.BASE_PATH
        + PredefinedAllowedAttributeValueApiPath.FILTER_ATTRIBUTE_ID_AND_VALUE_FOR_SUGGESTIONS)
        .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
        .param("requestId", REQUEST_ID).param("username", DEFAULT_USERNAME).param("value", VALUE)
        .accept(MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk());
    verify(this.service)
        .getBrandSuggestions(eq(STORE_ID), eq(VALUE), eq(NA), eq(pageable), eq(Boolean.FALSE), eq(Boolean.FALSE));
    verify(this.responsePage).getContent();
    verify(this.responsePage).getTotalElements();
  }

  @Test
  public void getBrandSuggestionsExceptionTest() throws Exception {
    Pageable pageable = PageRequest.of(0, 10);
    when(this.service.getBrandSuggestions(eq(STORE_ID), eq(VALUE), eq(NA), eq(pageable), eq(Boolean.FALSE), eq(Boolean.FALSE)))
        .thenThrow(ApplicationRuntimeException.class);
    when(this.responsePage.getContent()).thenReturn(this.responseList);
    when(this.responsePage.getTotalElements()).thenReturn(TOTAL_RECORDS);
    this.mockMvc.perform(get(PredefinedAllowedAttributeValueApiPath.BASE_PATH
        + PredefinedAllowedAttributeValueApiPath.FILTER_ATTRIBUTE_ID_AND_VALUE_FOR_SUGGESTIONS)
        .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
        .param("requestId", REQUEST_ID).param("username", DEFAULT_USERNAME).param("value", VALUE)
        .accept(MediaType.APPLICATION_JSON_VALUE)
        .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());
    verify(this.service)
        .getBrandSuggestions(eq(STORE_ID), eq(VALUE), eq(NA), eq(pageable), eq(Boolean.FALSE), eq(Boolean.FALSE));
  }

  @Test
  public void savePredefinedAllowedAttributeTest()
      throws Exception {
    PredefinedAllowedAttributeValueRequest predefinedAllowedAttributeValueRequest = new PredefinedAllowedAttributeValueRequest();
    predefinedAllowedAttributeValueRequest.setValue(VALUE);
    predefinedAllowedAttributeValueRequest.setPredefinedAllowedAttributeCode(ATTRIBUTE_CODE);
    predefinedAllowedAttributeValueRequest.setSequence(1);

    this.mockMvc.perform(MockMvcRequestBuilders.post(PredefinedAllowedAttributeValueApiPath.BASE_PATH
        + PredefinedAllowedAttributeValueApiPath.SAVE)
        .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
        .param("requestId", REQUEST_ID).param("username", DEFAULT_USERNAME)
        .param("attributeId", ID)
        .accept(MediaType.APPLICATION_JSON_VALUE)
        .contentType(MediaType.APPLICATION_JSON)
        .content(OBJECT_MAPPER.writeValueAsString(predefinedAllowedAttributeValueRequest)))
        .andExpect(status().isOk());
    verify(this.service)
        .saveWithGeneratedCode(eq(STORE_ID), eq(ID), Mockito.any(PredefinedAllowedAttributeValue.class));
  }

  @Test
  @SuppressWarnings("unchecked")
  public void populateListFromRequestTest()
      throws Exception {
    Object object = Class.forName("com.gdn.x.productcategorybase.controller.PredefinedAllowedAttributeValueController").newInstance();
    Method method = object.getClass().getDeclaredMethod("populateListFromRequest", String.class, String.class, String.class, String.class, List.class);
    Assertions.assertTrue(Modifier.isPrivate(method.getModifiers()));
    method.setAccessible(true);

    List<PredefinedAllowedAttributeValueRequest> predefinedAllowedAttributeValueRequests = new ArrayList<>();
    PredefinedAllowedAttributeValueRequest predefinedAllowedAttributeValueRequest = new PredefinedAllowedAttributeValueRequest();
    predefinedAllowedAttributeValueRequest.setValue(VALUE);
    predefinedAllowedAttributeValueRequest.setPredefinedAllowedAttributeCode(ATTRIBUTE_CODE);
    predefinedAllowedAttributeValueRequest.setSequence(1);
    predefinedAllowedAttributeValueRequests.add(predefinedAllowedAttributeValueRequest);

    List<PredefinedAllowedAttributeValue> test =(List<PredefinedAllowedAttributeValue>) method.invoke(object, "", "", "" ,"", predefinedAllowedAttributeValueRequests);
  }

  @BeforeEach
  public void setUp() throws Exception {
    initMocks(this);
    this.predefinedAllowedAttributeValueList = new ArrayList<>();
    PredefinedAllowedAttributeValue predefinedAllowedAttributeValue = new PredefinedAllowedAttributeValue();
    this.predefinedAllowedAttributeValueList.add(predefinedAllowedAttributeValue);
    this.mockMvc = standaloneSetup(this.controller).build();
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(responsePage, brandServiceBean, service);
  }
}
