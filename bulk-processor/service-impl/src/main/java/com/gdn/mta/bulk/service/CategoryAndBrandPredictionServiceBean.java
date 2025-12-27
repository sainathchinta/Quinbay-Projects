package com.gdn.mta.bulk.service;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.dto.BrandAndCategoryPredictionRequest;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.entity.BulkProcessData;
import com.gdn.mta.bulk.entity.BulkProcessImageQC;
import com.gdn.mta.bulk.models.BrandPredictionRequest;
import com.gdn.mta.bulk.util.BulkUpdateServiceUtil;
import com.gdn.mta.bulk.util.CommonUtils;
import com.gdn.mta.bulk.util.GenericBulkHeaders;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.productcategorybase.dto.response.CategoryHierarchyResponse;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Stream;

@Service
@Slf4j
public class CategoryAndBrandPredictionServiceBean implements CategoryAndBrandPredictionService {

    @Autowired
    private BulkProcessDataService bulkProcessDataService;

    @Autowired
    private BulkProcessService bulkProcessService;

    @Autowired
    private BulkProcessImageQCService bulkProcessImageQCService;

    @Autowired
    private ProductCategoryPredictionOutboundService productCategoryPredictionOutboundService;

    @Autowired
    private BrandPredictionOutboundService brandPredictionOutboundService;

    @Autowired
    private PCBOutboundService pcbOutboundService;

    @Autowired
    private ObjectMapper objectMapper;

    @Override
    public void process(BrandAndCategoryPredictionRequest brandAndCategoryPredictionRequest)
        throws Exception {

        BulkProcess bulkProcess =
          bulkProcessService.findByStoreIdAndBulkProcessCodeAndStatusAndMarkForDeleteFalse(
            brandAndCategoryPredictionRequest.getStoreId(),
            brandAndCategoryPredictionRequest.getBulkProcessCode(), BulkProcess.STATUS_IMAGE_PROCESSING);

        List<BulkProcessData> bulkProcessDataList =
          bulkProcessDataService.findByBulkProcessIdAndParentProductAndStatus(
            brandAndCategoryPredictionRequest.getStoreId(),
            brandAndCategoryPredictionRequest.getBulkProcessId(),
            brandAndCategoryPredictionRequest.getParentProduct(), BulkProcessData.STATUS_PENDING);
        bulkProcessDataList.addAll(
          bulkProcessDataService.findByBulkProcessIdAndParentProductAndStatus(
            brandAndCategoryPredictionRequest.getStoreId(),
            brandAndCategoryPredictionRequest.getBulkProcessId(),
            brandAndCategoryPredictionRequest.getParentProduct(), BulkProcessData.STATUS_FAIL));
        BulkProcessImageQC bulkProcessImageQC =
            bulkProcessImageQCService.fetchBulkProcessImageQCByBulkProcessCodeAndParentProduct(
                brandAndCategoryPredictionRequest.getStoreId(),
                brandAndCategoryPredictionRequest.getBulkProcessCode(),
                brandAndCategoryPredictionRequest.getParentProduct());

        if(Objects.isNull(bulkProcess)){
            bulkProcessImageQC.setCompleted(true);
            bulkProcessImageQCService.saveBulkProcessImageQc(Collections.singletonList(bulkProcessImageQC));
            throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED,Constant.ERROR_IN_CATEGORY_PREDICTION);
        }

        Map<String, String> dsResponse = new HashMap<>();
        StringBuilder errorMessage = new StringBuilder();
        String brandName = StringUtils.EMPTY;
        String cnCategoryCode = null;
        Map<String, String> categoryHierarchy = new HashMap<>();
        try {
            cnCategoryCode =
                productCategoryPredictionOutboundService.predictProductCategoriesByProductName(
                    brandAndCategoryPredictionRequest.getProductName(),
                    brandAndCategoryPredictionRequest.getExternalCategory(), dsResponse);
            List<CategoryHierarchyResponse> categoryHierarchyResponses =
                pcbOutboundService.filterCategoryHierarchyByCategoryCodes(
                    Collections.singletonList(cnCategoryCode));
            CommonUtils.populateCategoryHierarchyByCnCategoryCodeForGenericCreation(
                categoryHierarchyResponses, categoryHierarchy);
        } catch (Exception e) {
            log.error("Error on category prediction for productName : {}  externalCategory : {} "
                    + "errorMessage : {}", brandAndCategoryPredictionRequest.getProductName(),
                brandAndCategoryPredictionRequest.getExternalCategory(), e.getMessage(), e);
            errorMessage.append(Constant.ERROR_IN_CATEGORY_PREDICTION).append(Constant.PERIOD);
        }
        try {
            brandName = brandPredictionOutboundService.predictProductBrand(
                new BrandPredictionRequest(brandAndCategoryPredictionRequest.getProductName(),
                    brandAndCategoryPredictionRequest.getProductDescription()), dsResponse);
        } catch (Exception e) {
            errorMessage.append(Constant.ERROR_IN_BRAND_PREDICTION).append(Constant.PERIOD);
        }

        for (BulkProcessData bulkProcessData : bulkProcessDataList) {
            TypeReference<LinkedHashMap<String, Object>> typeRef =
                new TypeReference<LinkedHashMap<String, Object>>() {};
            LinkedHashMap<String, Object> rowDataJson =
                objectMapper.readValue(bulkProcessData.getBulkRequestData(), typeRef);
            StringBuilder dataTableErrorMessage = new StringBuilder(errorMessage.toString());
            dataTableErrorMessage.append(
              Optional.ofNullable(bulkProcessData.getErrorMessage()).orElse(StringUtils.EMPTY));
            if (StringUtils.isNotBlank(dataTableErrorMessage)) {
                BulkUpdateServiceUtil.setFinalStatusForInputFailure(bulkProcessData, bulkProcess,
                  dataTableErrorMessage.toString(), 0, 1);
            }
            rowDataJson.put(GenericBulkHeaders.BRAND, brandName);
            rowDataJson.put(GenericBulkHeaders.CATEGORY,
                categoryHierarchy.getOrDefault(GenericBulkHeaders.CATEGORY, ""));
            Stream.of(GenericBulkHeaders.C2, GenericBulkHeaders.CN)
                .filter(categoryHierarchy::containsKey)
                .forEach(category -> rowDataJson.put(category, categoryHierarchy.get(category)));
            bulkProcessData.setBulkRequestData(objectMapper.writeValueAsString(rowDataJson));
        }
        bulkProcessDataService.saveAndReturnBulkProcessData(bulkProcessDataList);
        bulkProcessImageQC.setDsResponse(objectMapper.writeValueAsString(dsResponse));
        bulkProcessImageQC.setCompleted(true);
        bulkProcessImageQCService.saveBulkProcessImageQc(Collections.singletonList(bulkProcessImageQC));
    }

}
