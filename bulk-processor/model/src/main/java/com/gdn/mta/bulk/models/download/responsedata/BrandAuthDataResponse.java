package com.gdn.mta.bulk.models.download.responsedata;

import java.util.List;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class BrandAuthDataResponse extends BulkDataResponse {
    List<BrandAuthResponse> brandAuthResponseList;
}
