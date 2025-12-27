package com.gdn.x.productcategorybase.controller.util;


import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;

import com.gdn.x.productcategorybase.AttributeType;
import com.gdn.x.productcategorybase.dto.AttributeSummaryDTO;
import com.gdn.x.productcategorybase.dto.request.AttributeAndValueByTypeRequest;
import com.gdn.x.productcategorybase.dto.response.AttributeDetailResponse;
import com.gdn.x.productcategorybase.entity.Attribute;
import com.gdn.x.productcategorybase.entity.PredefinedAllowedAttributeValue;

public class AttributeControllerUtilTest {


  private static final String ATTRIBUTE_CODE = "ATT";
  private static final String ATRTIBUTE_TYPE_DEF = "DEFINING_ATTRIBUTE";
  private static final String ATRTIBUTE_TYPE_DESC = "DESCRIPTIVE_ATTRIBUTE";
  private static final String ATRTIBUTE_TYPE_PRE = "PREDEFINED_ATTRIBUTE";
  private static final String ATTRIBUTE_NAME = "ATTRIBUTE_NAME";
  private static final boolean IS_BASIC_VIEW = true;
  private static final boolean IS_SKU_VALUE = true;
  private static final boolean MANDATORY = false;
  private static final boolean SCREENING_MANDATORY = true;
  private static final boolean VARIANT_CREATING_UI = true;
  private static final boolean VARIANT_CREATION = false;
  
  @InjectMocks
  private AttributeControllerUtil attributeControllerUtil;

  private Attribute attrHidden;
  private Attribute attrNotHidden;
  private PredefinedAllowedAttributeValue paavHidden;
  private PredefinedAllowedAttributeValue paavNotHidden;

  @BeforeEach
  void setUp() {
    attrHidden = mock(Attribute.class);
    when(attrHidden.isHideForSeller()).thenReturn(true);

    attrNotHidden = mock(Attribute.class);
    when(attrNotHidden.isHideForSeller()).thenReturn(false);

    paavHidden = mock(PredefinedAllowedAttributeValue.class);
    when(paavHidden.getAttribute()).thenReturn(attrHidden);

    paavNotHidden = mock(PredefinedAllowedAttributeValue.class);
    when(paavNotHidden.getAttribute()).thenReturn(attrNotHidden);
  }
  
  @Test
   void convertIntoAttributeSummaryResponse_HappyFlow_Success() {
    List<AttributeDetailResponse> response = AttributeControllerUtil
        .convertIntoAttributeSummaryResponse(this.attributeSummaryListBuilder());
    assertTrue(response.get(0).getAttributeType().equals(ATRTIBUTE_TYPE_DEF));
  }

  private List<AttributeSummaryDTO> attributeSummaryListBuilder() {
    AttributeSummaryDTO attr1 =
        new AttributeSummaryDTO(ATTRIBUTE_CODE, AttributeType.valueOf(ATRTIBUTE_TYPE_DEF), ATTRIBUTE_NAME,
          IS_BASIC_VIEW, IS_SKU_VALUE, MANDATORY, SCREENING_MANDATORY, VARIANT_CREATING_UI, true);
    AttributeSummaryDTO attr2 =
        new AttributeSummaryDTO(ATTRIBUTE_CODE, AttributeType.valueOf(ATRTIBUTE_TYPE_DESC), ATTRIBUTE_NAME,
          IS_BASIC_VIEW, IS_SKU_VALUE, MANDATORY, SCREENING_MANDATORY, VARIANT_CREATING_UI,
          VARIANT_CREATION);
    AttributeSummaryDTO attr3 =
        new AttributeSummaryDTO(ATTRIBUTE_CODE, AttributeType.valueOf(ATRTIBUTE_TYPE_PRE), ATTRIBUTE_NAME,
          IS_BASIC_VIEW, IS_SKU_VALUE, MANDATORY, SCREENING_MANDATORY, VARIANT_CREATING_UI,
          VARIANT_CREATION);
    List<AttributeSummaryDTO> attributeList = new ArrayList<AttributeSummaryDTO>();
    attributeList.add(attr1);
    attributeList.add(attr2);
    attributeList.add(attr3);
    return attributeList;
  }

}
