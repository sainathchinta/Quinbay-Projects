package com.gdn.x.product.rest.web.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;

import java.io.Serializable;

@JsonIgnoreProperties(ignoreUnknown = true)
public class WholesaleRule implements Serializable{

	private int minQuantity;
	private int maxQuantity;
	private double discountPercentage;
	private double finalPrice;
	
	public WholesaleRule() {}

	public WholesaleRule(int minQuantity, int maxQuantity, double discountPercentage,
			double finalPrice) {
		this.minQuantity = minQuantity;
		this.maxQuantity = maxQuantity;
		this.discountPercentage = discountPercentage;
		this.finalPrice = finalPrice;
	}

	@Override
	public boolean equals(Object obj) {
		return GdnObjects.equals(this, obj);
	}

	public double getDiscountPercentage() {
		return discountPercentage;
	}

	public double getFinalPrice() {
		return finalPrice;
	}

	public int getMaxQuantity() {
		return maxQuantity;
	}

	public int getMinQuantity() {
		return minQuantity;
	}

	@Override
	public int hashCode() {
		return GdnObjects.hashCode(this);
	}

	public void setDiscountPercentage(double discountPercentage) {
		this.discountPercentage = discountPercentage;
	}

	public void setFinalPrice(double finalPrice) {
		this.finalPrice = finalPrice;
	}

	public void setMaxQuantity(int maxQuantity) {
		this.maxQuantity = maxQuantity;
	}

	public void setMinQuantity(int minQuantity) {
		this.minQuantity = minQuantity;
	}

	@Override
	public String toString() {
		return String.format(
				"WholesaleRule [minQuantity=%s, maxQuantity=%s, discountPercentage=%s, finalPrice=%s]",
				this.minQuantity, this.maxQuantity, this.discountPercentage, this.finalPrice);
	}
}
