package com.gdn.partners.product.analytics.service.impl;

import com.gdn.partners.product.analytics.entity.DSExtractionEntity;
import com.gdn.partners.product.analytics.repository.DsExtractionAttributesRepository;
import com.gdn.partners.product.analytics.service.DsExtractedAttributeService;
import com.gdn.partners.product.analytics.service.cache.DsExtractedAttributeCacheableService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import model.AttributeUpdateEventModel;
import org.springframework.stereotype.Service;

import java.util.Objects;
import java.util.Optional;

import static com.gdn.partners.product.analytics.model.Constants.DS_EXTRACTION;

@Service
@Slf4j
@RequiredArgsConstructor
public class DsExtractedAttributeServiceImpl implements DsExtractedAttributeService {

  private final DsExtractionAttributesRepository dsExtractionAttributesRepository;
  private final DsExtractedAttributeCacheableService dsExtractedAttributeCacheableService;

  @Override
  public void updateDsExtractedAttribute(AttributeUpdateEventModel attributeUpdateEventModel) {
    if (Objects.nonNull(attributeUpdateEventModel.getUpdatedFields())
        && attributeUpdateEventModel.getUpdatedFields().contains(DS_EXTRACTION)) {
      DSExtractionEntity dsExtractionEntity =
          dsExtractionAttributesRepository.findByDsAttributeName(
              attributeUpdateEventModel.getDsAttributeName());
      if (Objects.isNull(dsExtractionEntity)) {
        if (!attributeUpdateEventModel.isDsExtraction()) {
          return;
        } else {
          dsExtractionEntity = new DSExtractionEntity();
        }
      }
      if (attributeUpdateEventModel.isDsExtraction()) {
        dsExtractionEntity.setAttributeCode(attributeUpdateEventModel.getAttributeCode());
        dsExtractionEntity.setDsAttributeName(attributeUpdateEventModel.getDsAttributeName());
        dsExtractionEntity.setMarkForDelete(false);
      } else {
        dsExtractionEntity.setMarkForDelete(true);
      }

      dsExtractionAttributesRepository.save(dsExtractionEntity);
      dsExtractedAttributeCacheableService.evictCacheByDsAttributeName(
          attributeUpdateEventModel.getDsAttributeName());
    }
  }

  @Override
  public DSExtractionEntity getDsExtractedAttributeByAttributeCode(String attributeCode) {
    Optional<DSExtractionEntity> dsExtractionEntityOptional =
        dsExtractionAttributesRepository.findByAttributeCode(attributeCode);
    if (dsExtractionEntityOptional.isPresent()) {
      return dsExtractionEntityOptional.get();
    } else {
      return null;
    }
  }
}
