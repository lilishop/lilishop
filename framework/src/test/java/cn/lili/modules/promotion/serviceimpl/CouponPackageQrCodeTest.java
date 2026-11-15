package cn.lili.modules.promotion.serviceimpl;

import cn.hutool.extra.qrcode.QrCodeUtil;
import cn.hutool.extra.qrcode.QrConfig;
import cn.lili.modules.promotion.entity.enums.CouponActivityTypeEnum;
import cn.lili.modules.wechat.util.WechatMpCodeUtil;
import cn.lili.common.exception.ServiceException;
import org.springframework.test.util.ReflectionTestUtils;
import cn.lili.modules.promotion.entity.dos.CouponActivity;
import cn.lili.modules.promotion.entity.vos.CouponPackageQrCodeVO;
import org.junit.jupiter.api.Test;

import javax.imageio.ImageIO;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.util.Base64;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

/**
 * 券礼包推广码回归测试，验证图片可解码且扫码落地链接保留完整活动ID，不连接数据库。
 *
 * @author Bulbasaur
 * @since 2026-11-15
 */
class CouponPackageQrCodeTest {

    /**
     * 验证响应只有一份 Data URI 前缀，并实际解码 PNG 和二维码，避免仅检查字符串格式。
     *
     * @throws Exception 图片解码失败时抛出
     */
    @Test
    void generatesReadablePngForCouponPackageLandingPage() throws Exception {
        String activityId = "2121689012752809985";
        CouponActivityServiceImpl service = spy(new CouponActivityServiceImpl());
        CouponActivity activity = new CouponActivity();
        activity.setCouponActivityType(CouponActivityTypeEnum.COUPON_PACKAGE.name());
        doReturn(activity).when(service).getById(activityId);
        WechatMpCodeUtil wechat = mock(WechatMpCodeUtil.class);
        ReflectionTestUtils.setField(service, "wechatMpCodeUtil", wechat);
        String sample = QrCodeUtil.generateAsBase64("sample", new QrConfig(280, 280), "png");
        when(wechat.createCode("pages/promotion/coupon-pack", "pack_" + activityId))
                .thenReturn(sample.substring(sample.indexOf(',') + 1));

        CouponPackageQrCodeVO result = service.getCouponPackageQrCode(activityId, "https://m.example.com/");

        String expectedUrl = "https://m.example.com/pages/promotion/coupon-pack?id=" + activityId;
        assertThat(result.getActivityId()).isEqualTo(activityId);
        assertThat(result.getH5Url()).isEqualTo(expectedUrl);
        String prefix = "data:image/png;base64,";
        assertThat(result.getQrCodeBase64()).startsWith(prefix);
        byte[] png = Base64.getDecoder().decode(result.getQrCodeBase64().substring(prefix.length()));
        BufferedImage image = ImageIO.read(new ByteArrayInputStream(png));
        assertThat(image).isNotNull();
        assertThat(image.getWidth()).isEqualTo(300);
        assertThat(image.getHeight()).isEqualTo(300);
        assertThat(QrCodeUtil.decode(image)).isEqualTo(expectedUrl);
        verify(wechat).createCode("pages/promotion/coupon-pack", "pack_" + activityId);
        assertThat(result.getMiniProgramCodeError()).isNull();
        byte[] miniCode = Base64.getDecoder().decode(result.getMiniProgramCodeBase64().substring(prefix.length()));
        assertThat(ImageIO.read(new ByteArrayInputStream(miniCode))).isNotNull();
    }

    /** 微信返回错误JSON或抛出异常时，H5结果仍可用，且不把错误内容作为图片返回。 */
    @Test
    void keepsH5CodeWhenWechatFails() {
        CouponActivityServiceImpl service = spy(new CouponActivityServiceImpl());
        CouponActivity activity = new CouponActivity();
        activity.setCouponActivityType(CouponActivityTypeEnum.COUPON_PACKAGE.name());
        doReturn(activity).when(service).getById("123");
        WechatMpCodeUtil wechat = mock(WechatMpCodeUtil.class);
        ReflectionTestUtils.setField(service, "wechatMpCodeUtil", wechat);
        when(wechat.createCode("pages/promotion/coupon-pack", "pack_123"))
                .thenReturn(Base64.getEncoder().encodeToString("{\"errcode\":40001}".getBytes(java.nio.charset.StandardCharsets.UTF_8)))
                .thenThrow(new IllegalStateException("微信不可用"));
        for (int attempt = 0; attempt < 2; attempt++) {
            CouponPackageQrCodeVO result = service.getCouponPackageQrCode("123", "https://m.example.com");
            assertThat(result.getQrCodeBase64()).startsWith("data:image/png;base64,");
            assertThat(result.getMiniProgramCodeBase64()).isNull();
            assertThat(result.getMiniProgramCodeError()).isNotBlank();
        }
    }

    /** 只有券礼包才能生成礼包推广码，其他活动不得触发微信调用或短链接写入。 */
    @Test
    void rejectsNonPackageActivities() {
        CouponActivityServiceImpl service = spy(new CouponActivityServiceImpl());
        CouponActivity activity = new CouponActivity();
        activity.setCouponActivityType(CouponActivityTypeEnum.REGISTERED.name());
        doReturn(activity).when(service).getById("123");
        WechatMpCodeUtil wechat = mock(WechatMpCodeUtil.class);
        ReflectionTestUtils.setField(service, "wechatMpCodeUtil", wechat);
        assertThatThrownBy(() -> service.getCouponPackageQrCode("123", "https://m.example.com"))
                .isInstanceOf(ServiceException.class);
        verifyNoInteractions(wechat);
    }
}
