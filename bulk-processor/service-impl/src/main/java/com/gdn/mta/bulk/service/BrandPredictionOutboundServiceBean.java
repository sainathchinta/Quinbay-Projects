package com.gdn.mta.bulk.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.dto.BrandDetailsResponse;
import com.gdn.mta.bulk.dto.BrandSuggestionResponse;
import com.gdn.mta.bulk.feignConfig.DsProtectedBrandFeign;
import com.gdn.mta.bulk.models.BrandPredictionRequest;
import com.gdn.partners.bulk.util.Constant;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Map;
import java.util.Optional;

@Service
@Slf4j
public class BrandPredictionOutboundServiceBean implements BrandPredictionOutboundService {

    @Autowired
    private DsProtectedBrandFeign dsProtectedBrandFeign;

    @Autowired
    private ObjectMapper objectMapper;

    @Override
    public String predictProductBrand(BrandPredictionRequest brandPredictionRequest,
        Map<String, String> dsResponse) throws Exception {
        BrandSuggestionResponse response =
            dsProtectedBrandFeign.predictProductBrandByProductName(brandPredictionRequest);
        log.info("ds brand response : {} for request : {} ", response, brandPredictionRequest);

        dsResponse.put(Constant.BRAND_RECOMMENDATION, objectMapper.writeValueAsString(response));

        if (ObjectUtils.isEmpty(response)) {
            log.error("Error on brand prediction for productName {} : description : {} ",
                brandPredictionRequest.getProductName(), brandPredictionRequest.getDescription());
            throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED,
                Constant.ERROR_IN_BRAND_PREDICTION);
        }
        String brandName =
            Optional.ofNullable(response.getBrandRecommendation()).orElse(new ArrayList<>())
                .stream().filter(brandDetail -> Constant.BRAND_SOURCE_FINAL_RECOMMENDATION.equals(
                    brandDetail.getSource())).map(BrandDetailsResponse::getBrandName).findFirst()
                .orElse(null);
        if (StringUtils.isEmpty(brandName)) {
            log.error("Error on brand prediction for productName {} : description : {} ",
                brandPredictionRequest.getProductName(), brandPredictionRequest.getDescription());
            throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED,
                Constant.ERROR_IN_BRAND_PREDICTION);
        }
        return brandName;
    }
}
