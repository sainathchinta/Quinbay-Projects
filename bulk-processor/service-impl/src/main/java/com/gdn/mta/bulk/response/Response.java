package com.gdn.mta.bulk.response;

import java.util.List;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class Response<T> {

    private Integer code;
    private String status;
    private T data;
    private Map<String, List<String>> errors;
    private Map<String, Object> metadata;
}
