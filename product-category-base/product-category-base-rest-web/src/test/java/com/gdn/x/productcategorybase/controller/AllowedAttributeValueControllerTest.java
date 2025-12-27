package com.gdn.x.productcategorybase.controller;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.setup.MockMvcBuilders.standaloneSetup;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.UUID;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.productcategorybase.AllowedAttributeValueApiPath;
import com.gdn.x.productcategorybase.AttributeType;
import com.gdn.x.productcategorybase.dto.AttributeOptionDTO;
import com.gdn.x.productcategorybase.dto.ListHolderRequest;
import com.gdn.x.productcategorybase.dto.allowedvalue.AllowedAttributeValueDtoResponse;
import com.gdn.x.productcategorybase.dto.allowedvalue.AllowedAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.allowedvalue.AllowedValueDto;
import com.gdn.x.productcategorybase.service.AllowedAttributeValueService;

public class AllowedAttributeValueControllerTest {
  
  @InjectMocks
  private AllowedAttributeValueController controller;
  @Mock
  private AllowedAttributeValueService allowedAttributeValueService;
  
  private MockMvc mockMvc;
  private static final ObjectMapper mapper = new ObjectMapper();

  private static final String STORE_ID = "10001";
  private static final String CLIENT_ID = "client-id";
  private static final String CHANNEL_ID = "channel-id";
  private static final String REQUEST_ID = "REQ-001";
  private static final String DEFAULT_USERNAME = "developer";
  
  private static final String ATTR_CODE = "WA-123";
  private static final String ATTR_TYPE = AttributeType.DEFINING_ATTRIBUTE.toString();
  private static final String ATTRIBUTE_CODE = "ATT";
  private static final String ATTR_ALLOWED_VALUE = "Black";
  private static final String ALLOWED_VALUE_CODE = "BL-001";
  private static final String ALLOWED_VALUE_ID = UUID.randomUUID().toString();
  private static final String KEYWORD = "Keyword";
  private static final String OPTION_VALUE = "OPT_VAL";
  private static final String OPTION_VALUE_V2 = "OPT_VAL_V2";
  private static final String OPTION_CODE = "OPT_CODE";
  private static final String OPTION_CODE_V2 = "OPT_CODE_V2";
  private static final int PAGE = 0;
  private static final int SIZE = 10;
  
  @BeforeEach
  public void initialize() {
    MockitoAnnotations.initMocks(this);
    this.mockMvc = standaloneSetup(this.controller).build();
  }
  
  @AfterEach
  public void tearDown(){
    Mockito.verifyNoMoreInteractions(allowedAttributeValueService);
  }
  
  private List<AllowedAttributeValueDtoResponse> buildAllowedAttributeValueResponse(){
    List<AllowedAttributeValueDtoResponse> response = new ArrayList<>();
    AllowedAttributeValueDtoResponse allowedAttrValueDto = new AllowedAttributeValueDtoResponse();
    allowedAttrValueDto.setAttributeCode(ATTR_CODE);
    allowedAttrValueDto.setAttributeType(ATTR_TYPE);
    AllowedValueDto allowedValueDto = new AllowedValueDto();
    allowedValueDto.setAllowedValueCode(ALLOWED_VALUE_CODE);
    allowedValueDto.setAllowedValueId(ALLOWED_VALUE_ID);
    allowedValueDto.setValue(ATTR_ALLOWED_VALUE);
    allowedAttrValueDto.setAllowedValue(Arrays.asList(allowedValueDto));
    response.add(allowedAttrValueDto);
    return response;
  }
  
  @Test
  public void getPredefinedAndDefiningAllowedAttributeValue_HappyFlow_Success() throws JsonProcessingException, Exception {
    ListHolderRequest<AllowedAttributeValueRequest> request = new ListHolderRequest<>();
    AllowedAttributeValueRequest allowedAttrReq = new AllowedAttributeValueRequest();
    allowedAttrReq.setAttributeCode(ATTR_CODE);
    allowedAttrReq.setAttributeType(ATTR_TYPE);
    allowedAttrReq.setValues(Arrays.asList(ATTR_ALLOWED_VALUE));
    request.setLists(Arrays.asList(allowedAttrReq));
    
    Mockito.when(allowedAttributeValueService.findAllowedPredefiningAndDefiningAttributeValue(Mockito.any()))
      .thenReturn(this.buildAllowedAttributeValueResponse());
    
    this.mockMvc.perform(post(AllowedAttributeValueApiPath.BASE_PATH + AllowedAttributeValueApiPath.FIND_ALLOWED_VALUES)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .content(mapper.writeValueAsString(request))
        .param("storeId", STORE_ID).param("channelId", CHANNEL_ID)
        .param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
        .param("username", DEFAULT_USERNAME))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    
    Mockito.verify(allowedAttributeValueService).findAllowedPredefiningAndDefiningAttributeValue(Mockito.any());
  }
  
  @Test
  public void getPredefinedAndDefiningAllowedAttributeValue_Error_False() throws JsonProcessingException, Exception {
    ListHolderRequest<AllowedAttributeValueRequest> request = new ListHolderRequest<>();
    AllowedAttributeValueRequest allowedAttrReq = new AllowedAttributeValueRequest();
    allowedAttrReq.setAttributeCode(ATTR_CODE);
    allowedAttrReq.setAttributeType(ATTR_TYPE);
    allowedAttrReq.setValues(Arrays.asList(ATTR_ALLOWED_VALUE));
    request.setLists(Arrays.asList(allowedAttrReq));
    
    Mockito.when(allowedAttributeValueService.findAllowedPredefiningAndDefiningAttributeValue(Mockito.any()))
      .thenThrow(Exception.class);
    
    this.mockMvc.perform(post(AllowedAttributeValueApiPath.BASE_PATH + AllowedAttributeValueApiPath.FIND_ALLOWED_VALUES)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .content(mapper.writeValueAsString(request))
        .param("storeId", STORE_ID).param("channelId", CHANNEL_ID)
        .param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
        .param("username", DEFAULT_USERNAME))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));
    
    Mockito.verify(allowedAttributeValueService).findAllowedPredefiningAndDefiningAttributeValue(Mockito.any());
  }
  
  @Test
  public void getAttributeOptionsTest_HappyFlow_Success() throws Exception {
    Pageable pageable = PageRequest.of(PAGE, SIZE);
    when(this.allowedAttributeValueService
        .getAttributeOptionsByAttributeCodeAndKeyword(ATTRIBUTE_CODE, KEYWORD, pageable))
            .thenReturn(this.attributeOptionsBuilder());
    this.mockMvc
        .perform(get(AllowedAttributeValueApiPath.BASE_PATH
            + AllowedAttributeValueApiPath.GET_ATTRIBUTE_OPTIONS).accept(MediaType.APPLICATION_JSON)
                .contentType(MediaType.APPLICATION_JSON).param("storeId", STORE_ID)
                .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
                .param("requestId", REQUEST_ID).param("username", DEFAULT_USERNAME)
                .param("attributeCode", ATTRIBUTE_CODE).param("keyword", KEYWORD))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(this.allowedAttributeValueService)
        .getAttributeOptionsByAttributeCodeAndKeyword(ATTRIBUTE_CODE, KEYWORD, pageable);
  }
  
  @Test
  public void getAttributeOptionsTest_Exception_Fail() throws Exception {
    Pageable pageable = PageRequest.of(PAGE, SIZE);
    when(this.allowedAttributeValueService
        .getAttributeOptionsByAttributeCodeAndKeyword(ATTRIBUTE_CODE, KEYWORD, pageable))
            .thenThrow(Exception.class);
    this.mockMvc
        .perform(get(AllowedAttributeValueApiPath.BASE_PATH
            + AllowedAttributeValueApiPath.GET_ATTRIBUTE_OPTIONS).accept(MediaType.APPLICATION_JSON)
                .contentType(MediaType.APPLICATION_JSON).param("storeId", STORE_ID)
                .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
                .param("requestId", REQUEST_ID).param("username", DEFAULT_USERNAME)
                .param("attributeCode", ATTRIBUTE_CODE).param("keyword", KEYWORD))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(false)));
    verify(this.allowedAttributeValueService)
        .getAttributeOptionsByAttributeCodeAndKeyword(ATTRIBUTE_CODE, KEYWORD, pageable);
  }
  
  private Page<AttributeOptionDTO> attributeOptionsBuilder(){
    List<AttributeOptionDTO> options = new ArrayList<>();
    AttributeOptionDTO opt1 = new AttributeOptionDTO(OPTION_VALUE, OPTION_CODE);
    AttributeOptionDTO opt2 = new AttributeOptionDTO(OPTION_VALUE_V2, OPTION_CODE_V2);
    options.add(opt1);
    options.add(opt2);
    Page<AttributeOptionDTO> optionsPage = new PageImpl<AttributeOptionDTO>(options, PageRequest.of(PAGE, SIZE), 2);
    return optionsPage;
  }
  
}
