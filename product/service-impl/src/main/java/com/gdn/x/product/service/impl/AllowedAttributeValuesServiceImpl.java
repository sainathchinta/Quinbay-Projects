package com.gdn.x.product.service.impl;

import static com.gdn.common.base.GdnPreconditions.checkArgument;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.gdn.x.product.dao.api.AllowedAttributeValuesRepository;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.enums.MasterDataAttributeType;
import com.gdn.x.product.model.entity.AllowedAttributeValues;
import com.gdn.x.product.model.entity.ValueSequence;
import com.gdn.x.product.outbound.api.ProductCategoryBaseOutbound;
import com.gdn.x.product.rest.web.model.dto.MasterDataAllowedAttributeValueDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataAttributeDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductAttributeDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductAttributeValueDTO;
import com.gdn.x.product.rest.web.model.response.ProductAndItemsResponse;
import com.gdn.x.product.service.api.AllowedAttributeValuesService;
import com.gdn.x.product.service.util.CommonUtil;
import com.gdn.x.productcategorybase.domain.event.model.AttributeDomainEventModel;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class AllowedAttributeValuesServiceImpl implements AllowedAttributeValuesService {
  private static final String ATTRIBUTE_CODE_MUST_NOT_BE_BLANK = "attributeCode must not be blank";
  @Autowired
  private AllowedAttributeValuesRepository allowedAttributeValuesRepository;
  @Autowired
  private ProductCategoryBaseOutbound productCategoryBaseOutbound;

  @Value("${enable.attribute.sorting}")
  private boolean enableAttributeSorting;


  @Override
  public void updateAttributeValueSequenceByAttributeCode(AttributeDomainEventModel attributeDomainEventModel)
      throws Exception {
    if (Constants.DEFINING_ATTRIBUTE.equals(attributeDomainEventModel.getAttributeType())) {
      checkArgument(StringUtils.isNotBlank(attributeDomainEventModel.getAttributeCode()),
          AllowedAttributeValuesServiceImpl.ATTRIBUTE_CODE_MUST_NOT_BE_BLANK);
      AttributeResponse attributeResponse = this.productCategoryBaseOutbound.getAttributeDetailByAttributeCode(
          attributeDomainEventModel.getAttributeCode());
      if (!attributeResponse.isMarkForDelete() && CollectionUtils.isNotEmpty(
          attributeResponse.getAllowedAttributeValues())) {
        AllowedAttributeValues allowedAttributeValues = Optional.ofNullable(
                allowedAttributeValuesRepository.findByStoreIdAndAttributeCodeAndMarkForDeleteFalse(
                    Constants.DEFAULT_STORE_ID, attributeDomainEventModel.getAttributeCode()))
            .orElse(new AllowedAttributeValues());
        allowedAttributeValues.setStoreId(attributeResponse.getStoreId());
        List<ValueSequence> valueSequences = getValueSequences(attributeResponse);
        allowedAttributeValues.setAttributeCode(attributeResponse.getAttributeCode());
        allowedAttributeValues.setValueSequences(valueSequences);
        allowedAttributeValuesRepository.save(allowedAttributeValues);
      }
    }
  }

  private List<ValueSequence> getValueSequences(AttributeResponse attributeResponse) {
    return attributeResponse.getAllowedAttributeValues().stream()
        .filter(allowedAttributeValueResponse -> !allowedAttributeValueResponse.isMarkForDelete()).map(
            allowedAttributeValueResponse -> new ValueSequence(allowedAttributeValueResponse.getAllowedAttributeCode(),
                allowedAttributeValueResponse.getSequence())).collect(Collectors.toList());
  }

  @Override
  public void sortDefiningAttribute(String storeId, ProductAndItemsResponse productAndItemsResponse) {
    try {
      if (enableAttributeSorting) {
        List<MasterDataProductAttributeDTO> masterDataProductAttributeDTOS =
                productAndItemsResponse.getProduct().getMasterDataProduct().getMasterDataProductAttributes();
        if (CollectionUtils.isNotEmpty(masterDataProductAttributeDTOS)) {
          Set<String> definingAttributeCodes =
                  masterDataProductAttributeDTOS.stream().map(MasterDataProductAttributeDTO::getMasterDataAttribute).filter(
                                  masterDataProductAttribute -> MasterDataAttributeType.DEFINING_ATTRIBUTE.equals(
                                          masterDataProductAttribute.getAttributeType())).map(MasterDataAttributeDTO::getAttributeCode)
                          .collect(Collectors.toSet());
          if (CollectionUtils.isNotEmpty(definingAttributeCodes)) {
            List<AllowedAttributeValues> allowedAttributeValuesList =
                    allowedAttributeValuesRepository.findByStoreIdAndAttributeCodeInAndMarkForDeleteFalse(storeId,
                            definingAttributeCodes);
            if (CollectionUtils.isNotEmpty(allowedAttributeValuesList)) {
              Map<String, Map<String, Integer>> attributeCodeAndValueSequenceMap = new HashMap<>();
              for (AllowedAttributeValues allowedAttributeValues : allowedAttributeValuesList) {
                Map<String, Integer> map = new HashMap<>();
                for (ValueSequence valueSequence : allowedAttributeValues.getValueSequences()) {
                  map.put(valueSequence.getAllowedAttributeValueCode(), valueSequence.getSequence());
                }
                attributeCodeAndValueSequenceMap.put(allowedAttributeValues.getAttributeCode(), map);
              }
              for (MasterDataProductAttributeDTO masterDataProductAttributeDTO : masterDataProductAttributeDTOS) {
                for (MasterDataProductAttributeValueDTO masterDataProductAttributeValueDTO : masterDataProductAttributeDTO.getMasterDataProductAttributeValues()) {
                  if (Objects.nonNull(attributeCodeAndValueSequenceMap.get(
                      masterDataProductAttributeDTO.getMasterDataAttribute().getAttributeCode()))) {
                    if (Objects.isNull(
                      masterDataProductAttributeValueDTO.getAllowedAttributeValue())) {
                      continue;
                    }
                    masterDataProductAttributeValueDTO.getAllowedAttributeValue().setSequence(
                        attributeCodeAndValueSequenceMap.get(
                            masterDataProductAttributeDTO.getMasterDataAttribute().getAttributeCode()).get(
                            Optional.ofNullable(masterDataProductAttributeValueDTO.getAllowedAttributeValue())
                                .map(MasterDataAllowedAttributeValueDTO::getAllowedAttributeValueCode)
                                .orElse(StringUtils.EMPTY)));

                  }
                }
              }
              productAndItemsResponse.getProduct().getMasterDataProduct().setSortedDefiningAttributes(
                      CommonUtil.getSortedDefiningAttributes(productAndItemsResponse.getProduct().getMasterDataProduct()));
            }
          }
        }
      }
    } catch (Exception e) {
      log.error("Exception on sorting the attributes for the given product : {}, error - ", productAndItemsResponse.getProduct()
              .getProductCode(), e);
    }
  }
}
