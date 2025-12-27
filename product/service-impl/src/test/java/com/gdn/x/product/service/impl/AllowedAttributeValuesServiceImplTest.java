package com.gdn.x.product.service.impl;

import static org.mockito.Mockito.verify;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.x.product.dao.api.AllowedAttributeValuesRepository;
import com.gdn.x.product.enums.MasterDataAttributeType;
import com.gdn.x.product.model.entity.AllowedAttributeValues;
import com.gdn.x.product.model.entity.MasterDataAttribute;
import com.gdn.x.product.model.entity.SystemParameter;
import com.gdn.x.product.model.entity.ValueSequence;
import com.gdn.x.product.outbound.impl.ProductCategoryBaseOutboundImpl;
import com.gdn.x.product.rest.web.model.dto.MasterDataAllowedAttributeValueDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataAttributeDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductAttributeDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductAttributeValueDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductDTO;
import com.gdn.x.product.rest.web.model.response.ProductAndItemsResponse;
import com.gdn.x.product.rest.web.model.response.ProductResponse;
import com.gdn.x.product.service.api.SystemParameterService;
import com.gdn.x.productcategorybase.domain.event.model.AttributeDomainEventModel;
import com.gdn.x.productcategorybase.dto.response.AllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;

public class AllowedAttributeValuesServiceImplTest {

  private static final String STORE_ID = "10001";
  private static final String ATTRIBUTE_CODE = "ATTRIBUTE_CODE";
  private static final String ATTRIBUTE_TYPE = "ATTRIBUTE_TYPE";
  private static final String DEFINING_ATTRIBUTE = "DEFINING_ATTRIBUTE";
  private static final boolean TRUE = true;
  private static final boolean FALSE = false;
  private static final String VALUE = "value";
  private static final Integer SEQUENCE = 10;
  private static final String ENABLE_ATTRIBUTE_SORTING = "enableAttributeSorting";


  @InjectMocks
  private AllowedAttributeValuesServiceImpl allowedAttributeValuesService;
  @Mock
  private ProductCategoryBaseOutboundImpl productCategoryBaseOutbound;
  @Mock
  private AllowedAttributeValuesRepository allowedAttributeValuesRepository;
  @Mock
  private SystemParameterService systemParameterService;


  private AttributeDomainEventModel attributeDomainEventModel;
  private AttributeResponse attributeResponse;
  private AllowedAttributeValues allowedAttributeValues;
  private AllowedAttributeValueResponse allowedAttributeValueResponse;
  private ValueSequence valueSequence;
  private ProductAndItemsResponse productAndItemsResponse;
  private SystemParameter systemParameter;
  private ProductResponse productResponse;
  private MasterDataProductDTO masterDataProductDTO;


  @BeforeEach
  public void setUp() {
    MockitoAnnotations.openMocks(this);
    attributeDomainEventModel = new AttributeDomainEventModel();
    attributeResponse = new AttributeResponse();
    allowedAttributeValues = new AllowedAttributeValues();
    allowedAttributeValueResponse = new AllowedAttributeValueResponse();
    valueSequence = new ValueSequence();
    productAndItemsResponse = new ProductAndItemsResponse();
    systemParameter = new SystemParameter();
    productResponse = new ProductResponse();
    masterDataProductDTO = new MasterDataProductDTO();
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(productCategoryBaseOutbound, allowedAttributeValuesRepository);
  }

  @Test
  public void updateAttributeValueSequenceByAttributeCodeTrueTest() throws Exception {
    List<AllowedAttributeValueResponse> allowedAttributeValueResponses = new ArrayList<>();
    allowedAttributeValueResponse.setAllowedAttributeCode(ATTRIBUTE_CODE);
    allowedAttributeValueResponse.setValue(VALUE);
    allowedAttributeValueResponse.setSequence(SEQUENCE);
    allowedAttributeValueResponses.add(allowedAttributeValueResponse);
    attributeDomainEventModel.setAttributeType(DEFINING_ATTRIBUTE);
    attributeDomainEventModel.setAttributeCode(ATTRIBUTE_CODE);
    attributeResponse.setMarkForDelete(FALSE);
    attributeResponse.setAllowedAttributeValues(allowedAttributeValueResponses);
    attributeResponse.setAttributeCode(ATTRIBUTE_CODE);
    attributeResponse.setStoreId(STORE_ID);
    List<ValueSequence> valueSequences = new ArrayList<>();
    valueSequence.setSequence(SEQUENCE);
    valueSequence.setAllowedAttributeValueCode(ATTRIBUTE_CODE);
    valueSequences.add(valueSequence);
    allowedAttributeValues.setValueSequences(valueSequences);
    allowedAttributeValues.setAttributeCode(ATTRIBUTE_CODE);
    Mockito.when(productCategoryBaseOutbound.getAttributeDetailByAttributeCode(ATTRIBUTE_CODE))
        .thenReturn(attributeResponse);
    Mockito.when(
            allowedAttributeValuesRepository.findByStoreIdAndAttributeCodeAndMarkForDeleteFalse(STORE_ID, ATTRIBUTE_CODE))
        .thenReturn(new AllowedAttributeValues());
    allowedAttributeValuesService.updateAttributeValueSequenceByAttributeCode(attributeDomainEventModel);
    verify(this.productCategoryBaseOutbound).getAttributeDetailByAttributeCode(ATTRIBUTE_CODE);
    verify(this.allowedAttributeValuesRepository).findByStoreIdAndAttributeCodeAndMarkForDeleteFalse(STORE_ID,
        ATTRIBUTE_CODE);
    verify(this.allowedAttributeValuesRepository).save(allowedAttributeValues);
  }

  @Test
  public void updateAttributeValueSequenceByAttributeCodeFalseTest1() throws Exception {
    List<AllowedAttributeValueResponse> allowedAttributeValueResponses = new ArrayList<>();
    allowedAttributeValueResponse.setAllowedAttributeCode(ATTRIBUTE_CODE);
    allowedAttributeValueResponse.setValue(VALUE);
    allowedAttributeValueResponse.setSequence(SEQUENCE);
    allowedAttributeValueResponse.setMarkForDelete(TRUE);
    allowedAttributeValueResponses.add(allowedAttributeValueResponse);
    attributeDomainEventModel.setAttributeType(ATTRIBUTE_TYPE);
    attributeDomainEventModel.setAttributeCode(ATTRIBUTE_CODE);
    attributeResponse.setMarkForDelete(TRUE);
    attributeResponse.setAllowedAttributeValues(allowedAttributeValueResponses);
    attributeResponse.setAttributeCode(ATTRIBUTE_CODE);
    attributeResponse.setStoreId(STORE_ID);
    List<ValueSequence> valueSequences = new ArrayList<>();
    valueSequence.setSequence(SEQUENCE);
    valueSequence.setAllowedAttributeValueCode(ATTRIBUTE_CODE);
    valueSequences.add(valueSequence);
    allowedAttributeValues.setValueSequences(valueSequences);
    allowedAttributeValues.setAttributeCode(ATTRIBUTE_CODE);
    allowedAttributeValuesService.updateAttributeValueSequenceByAttributeCode(attributeDomainEventModel);
  }

  @Test
  public void updateAttributeValueSequenceByAttributeCodeNullTest() throws Exception {
    List<AllowedAttributeValueResponse> allowedAttributeValueResponses = new ArrayList<>();
    allowedAttributeValueResponse.setAllowedAttributeCode(ATTRIBUTE_CODE);
    allowedAttributeValueResponse.setValue(VALUE);
    allowedAttributeValueResponse.setSequence(SEQUENCE);
    allowedAttributeValueResponse.setMarkForDelete(FALSE);
    allowedAttributeValueResponses.add(allowedAttributeValueResponse);
    attributeDomainEventModel.setAttributeType(DEFINING_ATTRIBUTE);
    attributeDomainEventModel.setAttributeCode(ATTRIBUTE_CODE);
    attributeResponse.setMarkForDelete(FALSE);
    attributeResponse.setAllowedAttributeValues(allowedAttributeValueResponses);
    attributeResponse.setAttributeCode(ATTRIBUTE_CODE);
    attributeResponse.setStoreId(STORE_ID);
    List<ValueSequence> valueSequences = new ArrayList<>();
    valueSequence.setSequence(SEQUENCE);
    valueSequence.setAllowedAttributeValueCode(ATTRIBUTE_CODE);
    valueSequences.add(valueSequence);
    allowedAttributeValues.setValueSequences(valueSequences);
    allowedAttributeValues.setAttributeCode(ATTRIBUTE_CODE);
    Mockito.when(productCategoryBaseOutbound.getAttributeDetailByAttributeCode(ATTRIBUTE_CODE))
        .thenReturn(attributeResponse);
    Mockito.when(
            allowedAttributeValuesRepository.findByStoreIdAndAttributeCodeAndMarkForDeleteFalse(STORE_ID, ATTRIBUTE_CODE))
        .thenReturn(null);
    allowedAttributeValuesService.updateAttributeValueSequenceByAttributeCode(attributeDomainEventModel);
    verify(this.productCategoryBaseOutbound).getAttributeDetailByAttributeCode(ATTRIBUTE_CODE);
    verify(this.allowedAttributeValuesRepository).findByStoreIdAndAttributeCodeAndMarkForDeleteFalse(STORE_ID,
        ATTRIBUTE_CODE);
    verify(this.allowedAttributeValuesRepository).save(allowedAttributeValues);
  }

  @Test
  public void updateAttributeValueSequenceByAttributeCodeFalseTest2() throws Exception {
    List<AllowedAttributeValueResponse> allowedAttributeValueResponses = new ArrayList<>();
    allowedAttributeValueResponse.setAllowedAttributeCode(ATTRIBUTE_CODE);
    allowedAttributeValueResponse.setValue(VALUE);
    allowedAttributeValueResponse.setSequence(SEQUENCE);
    allowedAttributeValueResponse.setMarkForDelete(FALSE);
    allowedAttributeValueResponses.add(allowedAttributeValueResponse);
    attributeDomainEventModel.setAttributeType(DEFINING_ATTRIBUTE);
    attributeDomainEventModel.setAttributeCode(ATTRIBUTE_CODE);
    attributeResponse.setMarkForDelete(TRUE);
    attributeResponse.setAllowedAttributeValues(allowedAttributeValueResponses);
    attributeResponse.setAttributeCode(ATTRIBUTE_CODE);
    attributeResponse.setStoreId(STORE_ID);
    List<ValueSequence> valueSequences = new ArrayList<>();
    valueSequence.setSequence(SEQUENCE);
    valueSequence.setAllowedAttributeValueCode(ATTRIBUTE_CODE);
    valueSequences.add(valueSequence);
    allowedAttributeValues.setValueSequences(valueSequences);
    allowedAttributeValues.setAttributeCode(ATTRIBUTE_CODE);
    Mockito.when(productCategoryBaseOutbound.getAttributeDetailByAttributeCode(ATTRIBUTE_CODE))
        .thenReturn(attributeResponse);
    allowedAttributeValuesService.updateAttributeValueSequenceByAttributeCode(attributeDomainEventModel);
    verify(this.productCategoryBaseOutbound).getAttributeDetailByAttributeCode(ATTRIBUTE_CODE);
  }

  @Test
  public void updateAttributeValueSequenceByAttributeCodeTrueTest2() throws Exception {
    List<AllowedAttributeValueResponse> allowedAttributeValueResponses = new ArrayList<>();
    allowedAttributeValueResponse.setAllowedAttributeCode(ATTRIBUTE_CODE);
    allowedAttributeValueResponse.setValue(VALUE);
    allowedAttributeValueResponse.setSequence(SEQUENCE);
    allowedAttributeValueResponse.setMarkForDelete(TRUE);
    allowedAttributeValueResponses.add(allowedAttributeValueResponse);
    attributeDomainEventModel.setAttributeType(DEFINING_ATTRIBUTE);
    attributeDomainEventModel.setAttributeCode(ATTRIBUTE_CODE);
    attributeResponse.setMarkForDelete(FALSE);
    attributeResponse.setAllowedAttributeValues(new ArrayList<>());
    attributeResponse.setAttributeCode(ATTRIBUTE_CODE);
    attributeResponse.setStoreId(STORE_ID);
    List<ValueSequence> valueSequences = new ArrayList<>();
    valueSequence.setSequence(SEQUENCE);
    valueSequence.setAllowedAttributeValueCode(ATTRIBUTE_CODE);
    valueSequences.add(valueSequence);
    allowedAttributeValues.setValueSequences(valueSequences);
    allowedAttributeValues.setAttributeCode(ATTRIBUTE_CODE);
    Mockito.when(productCategoryBaseOutbound.getAttributeDetailByAttributeCode(ATTRIBUTE_CODE))
        .thenReturn(attributeResponse);
    Mockito.when(
            allowedAttributeValuesRepository.findByStoreIdAndAttributeCodeAndMarkForDeleteFalse(STORE_ID, ATTRIBUTE_CODE))
        .thenReturn(new AllowedAttributeValues());
    allowedAttributeValuesService.updateAttributeValueSequenceByAttributeCode(attributeDomainEventModel);
    verify(this.productCategoryBaseOutbound).getAttributeDetailByAttributeCode(ATTRIBUTE_CODE);
  }

  @Test
  public void updateAttributeValueSequenceByAttributeCodeTrueTest3() throws Exception {
    List<AllowedAttributeValueResponse> allowedAttributeValueResponses = new ArrayList<>();
    allowedAttributeValueResponse.setAllowedAttributeCode(ATTRIBUTE_CODE);
    allowedAttributeValueResponse.setValue(VALUE);
    allowedAttributeValueResponse.setSequence(SEQUENCE);
    allowedAttributeValueResponse.setMarkForDelete(TRUE);
    allowedAttributeValueResponses.add(allowedAttributeValueResponse);
    attributeDomainEventModel.setAttributeType(DEFINING_ATTRIBUTE);
    attributeDomainEventModel.setAttributeCode(ATTRIBUTE_CODE);
    attributeResponse.setMarkForDelete(FALSE);
    attributeResponse.setAllowedAttributeValues(allowedAttributeValueResponses);
    attributeResponse.setStoreId(STORE_ID);
    attributeResponse.setAttributeCode(ATTRIBUTE_CODE);
    List<ValueSequence> valueSequences = new ArrayList<>();
    valueSequence.setSequence(SEQUENCE);
    valueSequence.setAllowedAttributeValueCode(ATTRIBUTE_CODE);
    valueSequences.add(valueSequence);
    allowedAttributeValues.setValueSequences(new ArrayList<>());
    allowedAttributeValues.setAttributeCode(ATTRIBUTE_CODE);
    Mockito.when(productCategoryBaseOutbound.getAttributeDetailByAttributeCode(ATTRIBUTE_CODE))
        .thenReturn(attributeResponse);
    Mockito.when(
            allowedAttributeValuesRepository.findByStoreIdAndAttributeCodeAndMarkForDeleteFalse(STORE_ID, ATTRIBUTE_CODE))
        .thenReturn(new AllowedAttributeValues());
    allowedAttributeValuesService.updateAttributeValueSequenceByAttributeCode(attributeDomainEventModel);
    verify(this.productCategoryBaseOutbound).getAttributeDetailByAttributeCode(ATTRIBUTE_CODE);
    verify(this.allowedAttributeValuesRepository).findByStoreIdAndAttributeCodeAndMarkForDeleteFalse(STORE_ID,
        ATTRIBUTE_CODE);
    verify(this.allowedAttributeValuesRepository).save(allowedAttributeValues);
  }

  @Test
  public void sortDefiningAttributeValuesTrueTest() {
    ReflectionTestUtils.setField(allowedAttributeValuesService, "enableAttributeSorting", true);
    MasterDataAllowedAttributeValueDTO masterDataAllowedAttributeValueDTO = new MasterDataAllowedAttributeValueDTO();
    MasterDataAttributeDTO masterDataAttributeDTO = new MasterDataAttributeDTO();
    List<AllowedAttributeValues> allowedAttributeValuesList = new ArrayList<>();
    MasterDataProductAttributeDTO masterDataProductAttributeDTO = new MasterDataProductAttributeDTO();
    List<MasterDataProductAttributeValueDTO> masterDataProductAttributeValueDTOS = new ArrayList<>();
    List<MasterDataProductAttributeDTO> masterDataProductAttributeDTOList1 = new ArrayList<>();
    productAndItemsResponse.setProduct(productResponse);
    masterDataProductAttributeDTO.setMasterDataProductAttributeValues(masterDataProductAttributeValueDTOS);
    masterDataProductAttributeDTOList1.add(masterDataProductAttributeDTO);
    masterDataProductAttributeDTO.setMasterDataAttribute(masterDataAttributeDTO);
    masterDataAttributeDTO.setAttributeCode(ATTRIBUTE_CODE);
    MasterDataAttribute masterDataAttribute1 = new MasterDataAttribute();
    masterDataAttribute1.setAttributeType(MasterDataAttributeType.DEFINING_ATTRIBUTE);
    masterDataAttributeDTO.setAttributeType(masterDataAttribute1.getAttributeType());
    Set<String> definingAttributeCodes = new HashSet<>();
    definingAttributeCodes.add(masterDataAttributeDTO.getAttributeCode());
    allowedAttributeValues.setValueSequences(new ArrayList<>());
    allowedAttributeValuesList.add(allowedAttributeValues);
    Map<String, Integer> map = new HashMap<>();
    valueSequence.setAllowedAttributeValueCode(ATTRIBUTE_CODE);
    valueSequence.setSequence(1);
    allowedAttributeValues.setAttributeCode(ATTRIBUTE_CODE);
    Map<String, Integer> mp = new HashMap<>();
    mp.put(valueSequence.getAllowedAttributeValueCode(), valueSequence.getSequence());
    Map<String, Map<String, Integer>> attributeCodeAndValueSequenceMap = new HashMap<>();
    attributeCodeAndValueSequenceMap.put(allowedAttributeValues.getAttributeCode(), mp);
    MasterDataProductAttributeValueDTO masterDataProductAttributeValueDTO = new MasterDataProductAttributeValueDTO();
    masterDataProductAttributeDTO.setMasterDataProductAttributeValues(masterDataProductAttributeValueDTOS);
    masterDataProductAttributeValueDTO.setAllowedAttributeValue(masterDataAllowedAttributeValueDTO);
    masterDataAllowedAttributeValueDTO.setSequence(10);
    masterDataProductAttributeValueDTO.setMarkForDelete(false);
    masterDataProductAttributeValueDTOS.add(masterDataProductAttributeValueDTO);
    masterDataProductDTO.setMasterDataProductAttributes(masterDataProductAttributeDTOList1);
    productResponse.setMasterDataProduct(masterDataProductDTO);

    Mockito.when(allowedAttributeValuesRepository.findByStoreIdAndAttributeCodeInAndMarkForDeleteFalse(STORE_ID,
        definingAttributeCodes)).thenReturn(allowedAttributeValuesList);
    allowedAttributeValuesService.sortDefiningAttribute(STORE_ID, productAndItemsResponse);
    verify(this.allowedAttributeValuesRepository).findByStoreIdAndAttributeCodeInAndMarkForDeleteFalse(STORE_ID,
        definingAttributeCodes);
  }

  @Test
  public void sortDefiningAttributeValuesTrueNullTest() {
    ReflectionTestUtils.setField(allowedAttributeValuesService, "enableAttributeSorting", true);
    MasterDataAllowedAttributeValueDTO masterDataAllowedAttributeValueDTO = new MasterDataAllowedAttributeValueDTO();
    MasterDataAttributeDTO masterDataAttributeDTO = new MasterDataAttributeDTO();
    List<AllowedAttributeValues> allowedAttributeValuesList = new ArrayList<>();
    MasterDataProductAttributeDTO masterDataProductAttributeDTO = new MasterDataProductAttributeDTO();
    List<MasterDataProductAttributeValueDTO> masterDataProductAttributeValueDTOS = new ArrayList<>();
    List<MasterDataProductAttributeDTO> masterDataProductAttributeDTOList1 = new ArrayList<>();
    productAndItemsResponse.setProduct(productResponse);
    masterDataProductAttributeDTO.setMasterDataProductAttributeValues(masterDataProductAttributeValueDTOS);
    masterDataProductAttributeDTOList1.add(masterDataProductAttributeDTO);
    masterDataProductAttributeDTO.setMasterDataAttribute(masterDataAttributeDTO);
    masterDataAttributeDTO.setAttributeCode(ATTRIBUTE_CODE);
    MasterDataAttribute masterDataAttribute1 = new MasterDataAttribute();
    masterDataAttribute1.setAttributeType(MasterDataAttributeType.DEFINING_ATTRIBUTE);
    masterDataAttributeDTO.setAttributeType(masterDataAttribute1.getAttributeType());
    Set<String> definingAttributeCodes = new HashSet<>();
    definingAttributeCodes.add(masterDataAttributeDTO.getAttributeCode());
    allowedAttributeValues.setValueSequences(new ArrayList<>());
    allowedAttributeValuesList.add(allowedAttributeValues);
    Map<String, Integer> map = new HashMap<>();
    valueSequence.setAllowedAttributeValueCode(ATTRIBUTE_CODE);
    valueSequence.setSequence(1);
    allowedAttributeValues.setAttributeCode(ATTRIBUTE_CODE);
    Map<String, Integer> mp = new HashMap<>();
    mp.put(valueSequence.getAllowedAttributeValueCode(), valueSequence.getSequence());
    Map<String, Map<String, Integer>> attributeCodeAndValueSequenceMap = new HashMap<>();
    attributeCodeAndValueSequenceMap.put(allowedAttributeValues.getAttributeCode(), mp);
    MasterDataProductAttributeValueDTO masterDataProductAttributeValueDTO = new MasterDataProductAttributeValueDTO();
    masterDataProductAttributeDTO.setMasterDataProductAttributeValues(masterDataProductAttributeValueDTOS);
    masterDataProductAttributeValueDTO.setAllowedAttributeValue(null);
    masterDataAllowedAttributeValueDTO.setSequence(10);
    masterDataProductAttributeValueDTO.setMarkForDelete(false);
    masterDataProductAttributeValueDTOS.add(masterDataProductAttributeValueDTO);
    masterDataProductDTO.setMasterDataProductAttributes(masterDataProductAttributeDTOList1);
    productResponse.setMasterDataProduct(masterDataProductDTO);

    Mockito.when(allowedAttributeValuesRepository.findByStoreIdAndAttributeCodeInAndMarkForDeleteFalse(STORE_ID,
      definingAttributeCodes)).thenReturn(allowedAttributeValuesList);
    allowedAttributeValuesService.sortDefiningAttribute(STORE_ID, productAndItemsResponse);
    verify(this.allowedAttributeValuesRepository).findByStoreIdAndAttributeCodeInAndMarkForDeleteFalse(STORE_ID,
      definingAttributeCodes);
  }


  @Test
  public void sortDefiningAttributeValuesFalseTest() {
    ReflectionTestUtils.setField(allowedAttributeValuesService, "enableAttributeSorting", false);
    MasterDataAttributeDTO masterDataAttributeDTO = new MasterDataAttributeDTO();
    List<AllowedAttributeValues> allowedAttributeValuesList = new ArrayList<>();
    MasterDataProductAttributeDTO masterDataProductAttributeDTO = new MasterDataProductAttributeDTO();
    List<MasterDataProductAttributeDTO> masterDataProductAttributeDTOList = new ArrayList<>();
    List<MasterDataProductAttributeDTO> masterDataProductAttributeDTOList1 = new ArrayList<>();
    productAndItemsResponse.setProduct(productResponse);
    productResponse.setMasterDataProduct(masterDataProductDTO);
    masterDataProductDTO.setMasterDataProductAttributes(masterDataProductAttributeDTOList1);
    masterDataProductAttributeDTOList1.add(masterDataProductAttributeDTO);
    masterDataProductAttributeDTO.setMasterDataAttribute(masterDataAttributeDTO);
    masterDataAttributeDTO.setAttributeCode(ATTRIBUTE_CODE);
    MasterDataAttribute masterDataAttribute1 = new MasterDataAttribute();
    masterDataAttribute1.setAttributeType(MasterDataAttributeType.DEFINING_ATTRIBUTE);
    masterDataAttributeDTO.setAttributeType(masterDataAttribute1.getAttributeType());
    Set<String> definingAttributeCodes = new HashSet<>();
    definingAttributeCodes.add(masterDataAttributeDTO.getAttributeCode());
    allowedAttributeValues.setValueSequences(new ArrayList<>());
    allowedAttributeValuesList.add(allowedAttributeValues);
    Map<String, Integer> map = new HashMap<>();
    valueSequence.setAllowedAttributeValueCode(ATTRIBUTE_CODE);
    valueSequence.setSequence(1);
    map.put(valueSequence.getAllowedAttributeValueCode(), valueSequence.getSequence());
    allowedAttributeValues.setAttributeCode(ATTRIBUTE_CODE);
    Map<String, Map<String, Integer>> attributeCodeAndValueSequenceMap = new HashMap<>();
    attributeCodeAndValueSequenceMap.put(allowedAttributeValues.getAttributeCode(), map);
    List<MasterDataProductAttributeValueDTO> masterDataProductAttributeValueDTOS = new ArrayList<>();
    masterDataProductAttributeDTO.setMasterDataProductAttributeValues(masterDataProductAttributeValueDTOS);
    allowedAttributeValuesService.sortDefiningAttribute(STORE_ID, productAndItemsResponse);
  }

  @Test
  public void sortDefiningAttributeValuesFalseTest2() {
    ReflectionTestUtils.setField(allowedAttributeValuesService, "enableAttributeSorting", true);
    MasterDataAttributeDTO masterDataAttributeDTO = new MasterDataAttributeDTO();
    List<AllowedAttributeValues> allowedAttributeValuesList = new ArrayList<>();
    MasterDataProductAttributeDTO masterDataProductAttributeDTO = new MasterDataProductAttributeDTO();
    List<MasterDataProductAttributeDTO> masterDataProductAttributeDTOList = new ArrayList<>();
    productAndItemsResponse.setProduct(productResponse);
    productResponse.setMasterDataProduct(masterDataProductDTO);
    masterDataProductAttributeDTO.setMasterDataAttribute(masterDataAttributeDTO);
    masterDataAttributeDTO.setAttributeCode(ATTRIBUTE_CODE);
    MasterDataAttribute masterDataAttribute1 = new MasterDataAttribute();
    masterDataAttribute1.setAttributeType(MasterDataAttributeType.DEFINING_ATTRIBUTE);
    masterDataAttributeDTO.setAttributeType(masterDataAttribute1.getAttributeType());
    Set<String> definingAttributeCodes = new HashSet<>();
    definingAttributeCodes.add(masterDataAttributeDTO.getAttributeCode());
    allowedAttributeValues.setValueSequences(new ArrayList<>());
    allowedAttributeValuesList.add(allowedAttributeValues);
    Map<String, Integer> map = new HashMap<>();
    valueSequence.setAllowedAttributeValueCode(ATTRIBUTE_CODE);
    valueSequence.setSequence(1);
    map.put(valueSequence.getAllowedAttributeValueCode(), valueSequence.getSequence());
    allowedAttributeValues.setAttributeCode(ATTRIBUTE_CODE);
    Map<String, Map<String, Integer>> attributeCodeAndValueSequenceMap = new HashMap<>();
    attributeCodeAndValueSequenceMap.put(allowedAttributeValues.getAttributeCode(), map);
    List<MasterDataProductAttributeValueDTO> masterDataProductAttributeValueDTOS = new ArrayList<>();
    masterDataProductAttributeDTO.setMasterDataProductAttributeValues(masterDataProductAttributeValueDTOS);
    allowedAttributeValuesService.sortDefiningAttribute(STORE_ID, productAndItemsResponse);
  }

  @Test
  public void sortDefiningAttributeValuesFalseTest3() {
    ReflectionTestUtils.setField(allowedAttributeValuesService, "enableAttributeSorting", true);
    MasterDataAttributeDTO masterDataAttributeDTO = new MasterDataAttributeDTO();
    List<AllowedAttributeValues> allowedAttributeValuesList = new ArrayList<>();
    MasterDataProductAttributeDTO masterDataProductAttributeDTO = new MasterDataProductAttributeDTO();
    List<MasterDataProductAttributeDTO> masterDataProductAttributeDTOList1 = new ArrayList<>();
    productAndItemsResponse.setProduct(productResponse);
    productResponse.setMasterDataProduct(masterDataProductDTO);
    masterDataProductDTO.setMasterDataProductAttributes(masterDataProductAttributeDTOList1);
    masterDataProductAttributeDTOList1.add(masterDataProductAttributeDTO);
    masterDataProductAttributeDTO.setMasterDataAttribute(masterDataAttributeDTO);
    masterDataAttributeDTO.setAttributeCode(ATTRIBUTE_CODE);
    MasterDataAttribute masterDataAttribute1 = new MasterDataAttribute();
    masterDataAttribute1.setAttributeType(MasterDataAttributeType.DESCRIPTIVE_ATTRIBUTE);
    masterDataAttributeDTO.setAttributeType(masterDataAttribute1.getAttributeType());
    allowedAttributeValues.setValueSequences(new ArrayList<>());
    allowedAttributeValuesList.add(allowedAttributeValues);
    Map<String, Integer> map = new HashMap<>();
    valueSequence.setAllowedAttributeValueCode(ATTRIBUTE_CODE);
    valueSequence.setSequence(1);
    map.put(valueSequence.getAllowedAttributeValueCode(), valueSequence.getSequence());
    allowedAttributeValues.setAttributeCode(ATTRIBUTE_CODE);
    Map<String, Map<String, Integer>> attributeCodeAndValueSequenceMap = new HashMap<>();
    attributeCodeAndValueSequenceMap.put(allowedAttributeValues.getAttributeCode(), map);
    List<MasterDataProductAttributeValueDTO> masterDataProductAttributeValueDTOS = new ArrayList<>();
    masterDataProductAttributeDTO.setMasterDataProductAttributeValues(masterDataProductAttributeValueDTOS);
    allowedAttributeValuesService.sortDefiningAttribute(STORE_ID, productAndItemsResponse);
  }

  @Test
  public void sortDefiningAttributeValuesFalseTest4() {
    ReflectionTestUtils.setField(allowedAttributeValuesService, "enableAttributeSorting", true);
    MasterDataAttributeDTO masterDataAttributeDTO = new MasterDataAttributeDTO();
    List<AllowedAttributeValues> allowedAttributeValuesList = new ArrayList<>();
    MasterDataProductAttributeDTO masterDataProductAttributeDTO = new MasterDataProductAttributeDTO();
    List<MasterDataProductAttributeDTO> masterDataProductAttributeDTOList1 = new ArrayList<>();
    productAndItemsResponse.setProduct(productResponse);
    productResponse.setMasterDataProduct(masterDataProductDTO);
    masterDataProductDTO.setMasterDataProductAttributes(masterDataProductAttributeDTOList1);
    masterDataProductAttributeDTOList1.add(masterDataProductAttributeDTO);
    masterDataProductAttributeDTO.setMasterDataAttribute(masterDataAttributeDTO);
    masterDataAttributeDTO.setAttributeCode(ATTRIBUTE_CODE);
    MasterDataAttribute masterDataAttribute1 = new MasterDataAttribute();
    masterDataAttribute1.setAttributeType(MasterDataAttributeType.DEFINING_ATTRIBUTE);
    masterDataAttributeDTO.setAttributeType(masterDataAttribute1.getAttributeType());
    Set<String> definingAttributeCodes = new HashSet<>();
    definingAttributeCodes.add(masterDataAttributeDTO.getAttributeCode());
    allowedAttributeValues.setValueSequences(new ArrayList<>());
    Map<String, Integer> map = new HashMap<>();
    valueSequence.setAllowedAttributeValueCode(ATTRIBUTE_CODE);
    valueSequence.setSequence(1);
    map.put(valueSequence.getAllowedAttributeValueCode(), valueSequence.getSequence());
    allowedAttributeValues.setAttributeCode(ATTRIBUTE_CODE);
    Map<String, Map<String, Integer>> attributeCodeAndValueSequenceMap = new HashMap<>();
    attributeCodeAndValueSequenceMap.put(allowedAttributeValues.getAttributeCode(), map);
    List<MasterDataProductAttributeValueDTO> masterDataProductAttributeValueDTOS = new ArrayList<>();
    masterDataProductAttributeDTO.setMasterDataProductAttributeValues(masterDataProductAttributeValueDTOS);
    Mockito.when(allowedAttributeValuesRepository.findByStoreIdAndAttributeCodeInAndMarkForDeleteFalse(STORE_ID,
        definingAttributeCodes)).thenReturn(allowedAttributeValuesList);
    allowedAttributeValuesService.sortDefiningAttribute(STORE_ID, productAndItemsResponse);
    verify(this.allowedAttributeValuesRepository).findByStoreIdAndAttributeCodeInAndMarkForDeleteFalse(STORE_ID,
        definingAttributeCodes);
  }

  @Test
  public void sortDefiningAttributeValuesFalseTest5() {
    ReflectionTestUtils.setField(allowedAttributeValuesService, "enableAttributeSorting", true);
    MasterDataAttributeDTO masterDataAttributeDTO = new MasterDataAttributeDTO();
    List<AllowedAttributeValues> allowedAttributeValuesList = new ArrayList<>();
    allowedAttributeValues.setAttributeCode(ATTRIBUTE_CODE);
    MasterDataProductAttributeDTO masterDataProductAttributeDTO = new MasterDataProductAttributeDTO();
    List<MasterDataProductAttributeDTO> masterDataProductAttributeDTOList1 = new ArrayList<>();
    productAndItemsResponse.setProduct(productResponse);
    productResponse.setMasterDataProduct(masterDataProductDTO);
    masterDataProductDTO.setMasterDataProductAttributes(masterDataProductAttributeDTOList1);
    masterDataProductAttributeDTOList1.add(masterDataProductAttributeDTO);
    masterDataProductAttributeDTO.setMasterDataAttribute(masterDataAttributeDTO);
    masterDataAttributeDTO.setAttributeCode(ATTRIBUTE_CODE);
    MasterDataAttribute masterDataAttribute1 = new MasterDataAttribute();
    masterDataAttribute1.setAttributeType(MasterDataAttributeType.DEFINING_ATTRIBUTE);
    masterDataAttributeDTO.setAttributeType(masterDataAttribute1.getAttributeType());
    Set<String> definingAttributeCodes = new HashSet<>();
    definingAttributeCodes.add(masterDataAttributeDTO.getAttributeCode());
    Map<String, Integer> map = new HashMap<>();
    List<ValueSequence> valueSequences = new ArrayList<>();
    valueSequence.setAllowedAttributeValueCode(ATTRIBUTE_CODE);
    valueSequence.setSequence(1);
    valueSequences.add(valueSequence);
    allowedAttributeValues.setValueSequences(valueSequences);
    allowedAttributeValuesList.add(allowedAttributeValues);
    map.put(valueSequence.getAllowedAttributeValueCode(), valueSequence.getSequence());
    allowedAttributeValues.setAttributeCode(ATTRIBUTE_CODE);
    Map<String, Map<String, Integer>> attributeCodeAndValueSequenceMap = new HashMap<>();
    attributeCodeAndValueSequenceMap.put(allowedAttributeValues.getAttributeCode(), map);
    List<MasterDataProductAttributeValueDTO> masterDataProductAttributeValueDTOS = new ArrayList<>();
    masterDataProductAttributeDTO.setMasterDataProductAttributeValues(masterDataProductAttributeValueDTOS);
    Mockito.when(allowedAttributeValuesRepository.findByStoreIdAndAttributeCodeInAndMarkForDeleteFalse(STORE_ID,
        definingAttributeCodes)).thenReturn(allowedAttributeValuesList);
    allowedAttributeValuesService.sortDefiningAttribute(STORE_ID, productAndItemsResponse);
    verify(this.allowedAttributeValuesRepository).findByStoreIdAndAttributeCodeInAndMarkForDeleteFalse(STORE_ID,
        definingAttributeCodes);
  }

  @Test
  public void sortDefiningAttributeValuesMapNullTest() {
    ReflectionTestUtils.setField(allowedAttributeValuesService, "enableAttributeSorting", true);
    MasterDataAllowedAttributeValueDTO masterDataAllowedAttributeValueDTO = new MasterDataAllowedAttributeValueDTO();
    MasterDataAttributeDTO masterDataAttributeDTO = new MasterDataAttributeDTO();
    List<AllowedAttributeValues> allowedAttributeValuesList = new ArrayList<>();
    MasterDataProductAttributeDTO masterDataProductAttributeDTO = new MasterDataProductAttributeDTO();
    List<MasterDataProductAttributeValueDTO> masterDataProductAttributeValueDTOS = new ArrayList<>();
    List<MasterDataProductAttributeDTO> masterDataProductAttributeDTOList1 = new ArrayList<>();
    productAndItemsResponse.setProduct(productResponse);
    masterDataProductAttributeDTO.setMasterDataProductAttributeValues(masterDataProductAttributeValueDTOS);
    masterDataProductAttributeDTOList1.add(masterDataProductAttributeDTO);
    masterDataProductAttributeDTO.setMasterDataAttribute(masterDataAttributeDTO);
    masterDataAttributeDTO.setAttributeCode(ATTRIBUTE_TYPE);
    MasterDataAttribute masterDataAttribute1 = new MasterDataAttribute();
    masterDataAttribute1.setAttributeType(MasterDataAttributeType.DEFINING_ATTRIBUTE);
    masterDataAttributeDTO.setAttributeType(masterDataAttribute1.getAttributeType());
    Set<String> definingAttributeCodes = new HashSet<>();
    definingAttributeCodes.add(masterDataAttributeDTO.getAttributeCode());
    allowedAttributeValues.setValueSequences(new ArrayList<>());
    allowedAttributeValuesList.add(allowedAttributeValues);
    Map<String, Integer> map = new HashMap<>();
    valueSequence.setAllowedAttributeValueCode(ATTRIBUTE_CODE);
    valueSequence.setSequence(1);
    allowedAttributeValues.setAttributeCode(ATTRIBUTE_CODE);
    Map<String, Integer> mp = new HashMap<>();
    mp.put(valueSequence.getAllowedAttributeValueCode(), valueSequence.getSequence());
    Map<String, Map<String, Integer>> attributeCodeAndValueSequenceMap = new HashMap<>();
    attributeCodeAndValueSequenceMap.put(allowedAttributeValues.getAttributeCode(), mp);
    MasterDataProductAttributeValueDTO masterDataProductAttributeValueDTO = new MasterDataProductAttributeValueDTO();
    masterDataProductAttributeDTO.setMasterDataProductAttributeValues(masterDataProductAttributeValueDTOS);
    masterDataProductAttributeValueDTO.setAllowedAttributeValue(masterDataAllowedAttributeValueDTO);
    masterDataAllowedAttributeValueDTO.setSequence(10);
    masterDataProductAttributeValueDTO.setMarkForDelete(false);
    masterDataProductAttributeValueDTOS.add(masterDataProductAttributeValueDTO);
    masterDataProductDTO.setMasterDataProductAttributes(masterDataProductAttributeDTOList1);
    productResponse.setMasterDataProduct(masterDataProductDTO);

    Mockito.when(allowedAttributeValuesRepository.findByStoreIdAndAttributeCodeInAndMarkForDeleteFalse(STORE_ID,
        definingAttributeCodes)).thenReturn(allowedAttributeValuesList);
    allowedAttributeValuesService.sortDefiningAttribute(STORE_ID, productAndItemsResponse);
    verify(this.allowedAttributeValuesRepository).findByStoreIdAndAttributeCodeInAndMarkForDeleteFalse(STORE_ID,
        definingAttributeCodes);
  }

  @Test
  public void sortDefiningAttributeValuesExceptionTest() {
    ReflectionTestUtils.setField(allowedAttributeValuesService, "enableAttributeSorting", true);
    Assertions.assertThrows(Exception.class,
        () -> allowedAttributeValuesService.sortDefiningAttribute(STORE_ID, productAndItemsResponse));
  }
}
