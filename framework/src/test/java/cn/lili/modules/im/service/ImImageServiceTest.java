package cn.lili.modules.im.service;

import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.common.utils.SnowFlake;
import cn.lili.modules.file.plugin.FilePlugin;
import cn.lili.modules.im.entity.dos.ImImage;
import cn.lili.modules.im.entity.dos.ImTalk;
import cn.lili.modules.im.entity.vo.MessageOperation;
import cn.lili.modules.im.mapper.ImImageMapper;
import cn.lili.modules.system.entity.dto.OssSetting;
import com.baomidou.mybatisplus.core.MybatisConfiguration;
import com.baomidou.mybatisplus.core.conditions.Wrapper;
import com.baomidou.mybatisplus.core.metadata.TableInfoHelper;
import org.apache.ibatis.builder.MapperBuilderAssistant;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.mock.web.MockMultipartFile;

import javax.imageio.ImageIO;
import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.*;

/**
 * 客服会话图片上传与消息校验服务单元测试
 *
 * @author Bulbasaur
 * @since 2026-11-11
 */
@ExtendWith(MockitoExtension.class)
class ImImageServiceTest {

    @Mock
    private ImTalkService imTalkService;

    @Mock
    private ImImageMapper imageMapper;

    @Mock
    private ImImageStorageSettings storageSettings;

    @Mock
    private FilePlugin filePlugin;

    private ImImageService imImageService;

    private byte[] validPngBytes;

    @BeforeAll
    static void initMyBatisPlus() {
        MapperBuilderAssistant assistant = new MapperBuilderAssistant(new MybatisConfiguration(), "");
        TableInfoHelper.initTableInfo(assistant, ImImage.class);
        SnowFlake.initialize(1, 1);
    }

    @BeforeEach
    void setUp() throws IOException {
        imImageService = spy(new ImImageService(imTalkService, imageMapper, storageSettings));

        BufferedImage img = new BufferedImage(2, 2, BufferedImage.TYPE_INT_RGB);
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        ImageIO.write(img, "png", baos);
        validPngBytes = baos.toByteArray();
    }

    private ImTalk createValidTalk() {
        ImTalk talk = new ImTalk();
        talk.setId("1001");
        talk.setUserId1("101");
        talk.setStoreFlag1(false);
        talk.setUserId2("201");
        talk.setStoreFlag2(true);
        talk.setDeleteFlag(false);
        return talk;
    }

    private AuthUser createMemberUser() {
        AuthUser user = new AuthUser();
        user.setId("101");
        user.setUsername("buyer");
        user.setRole(UserEnums.MEMBER);
        return user;
    }

    private AuthUser createStoreUser() {
        AuthUser user = new AuthUser();
        user.setId("901");
        user.setUsername("seller");
        user.setRole(UserEnums.STORE);
        user.setStoreId("201");
        return user;
    }

    @Test
    @DisplayName("上传-登录失效或会话ID非法拦截")
    void uploadThrowsWhenUserOrTalkIdInvalid() {
        MockMultipartFile file = new MockMultipartFile("file", "test.png", "image/png", validPngBytes);

        ServiceException ex1 = assertThrows(ServiceException.class, () ->
                imImageService.upload(null, "1001", file));
        assertThat(ex1.getMessage()).contains("登录失效或客服会话无效");

        AuthUser user = createMemberUser();
        ServiceException ex2 = assertThrows(ServiceException.class, () ->
                imImageService.upload(user, "not-a-number", file));
        assertThat(ex2.getMessage()).contains("登录失效或客服会话无效");
    }

    @Test
    @DisplayName("上传-会话不存在或已删除拦截")
    void uploadThrowsWhenTalkNotFoundOrDeleted() {
        AuthUser user = createMemberUser();
        MockMultipartFile file = new MockMultipartFile("file", "test.png", "image/png", validPngBytes);

        when(imTalkService.getById("1001")).thenReturn(null);
        ServiceException ex1 = assertThrows(ServiceException.class, () ->
                imImageService.upload(user, "1001", file));
        assertThat(ex1.getMessage()).contains("客服会话不存在");

        ImTalk deletedTalk = createValidTalk();
        deletedTalk.setDeleteFlag(true);
        when(imTalkService.getById("1001")).thenReturn(deletedTalk);
        ServiceException ex2 = assertThrows(ServiceException.class, () ->
                imImageService.upload(user, "1001", file));
        assertThat(ex2.getMessage()).contains("客服会话不存在");
    }

    @Test
    @DisplayName("上传-非会话参与人拦截")
    void uploadThrowsWhenUserNotParticipant() {
        AuthUser stranger = new AuthUser();
        stranger.setId("999");
        stranger.setRole(UserEnums.MEMBER);

        ImTalk talk = createValidTalk();
        when(imTalkService.getById("1001")).thenReturn(talk);

        MockMultipartFile file = new MockMultipartFile("file", "test.png", "image/png", validPngBytes);
        ServiceException ex = assertThrows(ServiceException.class, () ->
                imImageService.upload(stranger, "1001", file));
        assertThat(ex.getMessage()).contains("无权访问该客服会话");
    }

    @Test
    @DisplayName("上传-非图片或损坏文件拦截")
    void uploadThrowsWhenInvalidImageFormat() {
        AuthUser user = createMemberUser();
        ImTalk talk = createValidTalk();
        when(imTalkService.getById("1001")).thenReturn(talk);

        MockMultipartFile emptyFile = new MockMultipartFile("file", "empty.png", "image/png", new byte[0]);
        ServiceException ex1 = assertThrows(ServiceException.class, () ->
                imImageService.upload(user, "1001", emptyFile));
        assertThat(ex1.getMessage()).contains("请选择不超过10MB的图片");

        MockMultipartFile corruptFile = new MockMultipartFile("file", "corrupt.png", "image/png", "not an image".getBytes(StandardCharsets.UTF_8));
        ServiceException ex2 = assertThrows(ServiceException.class, () ->
                imImageService.upload(user, "1001", corruptFile));
        assertThat(ex2.getMessage()).contains("仅支持有效的 PNG、JPG、GIF 或 WebP 图片");
    }

    @Test
    @DisplayName("上传-买家上传PNG成功并生成正确路径快照")
    void uploadMemberPngSuccess() {
        AuthUser user = createMemberUser();
        ImTalk talk = createValidTalk();
        when(imTalkService.getById("1001")).thenReturn(talk);

        OssSetting ossSetting = new OssSetting();
        ossSetting.setAliyunOSSBucketName("im-bucket");
        ossSetting.setAliyunOSSEndPoint("oss-cn-beijing.aliyuncs.com");
        when(storageSettings.getStorage()).thenReturn(ossSetting);

        doReturn(filePlugin).when(imImageService).createStorage(ossSetting);
        when(filePlugin.inputStreamUpload(any(), any(), eq("image/png"))).thenReturn("https://im-bucket.oss.com/test.png");
        when(imageMapper.insert(any(ImImage.class))).thenReturn(1);

        MockMultipartFile file = new MockMultipartFile("file", "test.png", "image/png", validPngBytes);
        String url = imImageService.upload(user, "1001", file);

        assertThat(url).isEqualTo("https://im-bucket.oss.com/test.png");

        ArgumentCaptor<ImImage> captor = ArgumentCaptor.forClass(ImImage.class);
        verify(imageMapper).insert(captor.capture());
        ImImage saved = captor.getValue();
        assertThat(saved.getStoreId()).isEqualTo("201");
        assertThat(saved.getTalkId()).isEqualTo("1001");
        assertThat(saved.getUploaderId()).isEqualTo("101");
        assertThat(saved.getUploaderRole()).isEqualTo("MEMBER");
        assertThat(saved.getBucket()).isEqualTo("im-bucket");
        assertThat(saved.getEndpoint()).isEqualTo("oss-cn-beijing.aliyuncs.com");
        assertThat(saved.getContentType()).isEqualTo("image/png");
        assertThat(saved.getUrl()).isEqualTo("https://im-bucket.oss.com/test.png");
        assertThat(saved.getObjectKey()).startsWith("im/store/201/talk/1001/");
        assertThat(saved.getObjectKey()).endsWith(".png");
    }

    @Test
    @DisplayName("上传-支持WebP格式识别与上传")
    void uploadWebpSuccess() {
        AuthUser user = createStoreUser();
        ImTalk talk = createValidTalk();
        when(imTalkService.getById("1001")).thenReturn(talk);

        OssSetting ossSetting = new OssSetting();
        ossSetting.setAliyunOSSBucketName("im-bucket");
        ossSetting.setAliyunOSSEndPoint("oss-cn-beijing.aliyuncs.com");
        when(storageSettings.getStorage()).thenReturn(ossSetting);

        doReturn(filePlugin).when(imImageService).createStorage(ossSetting);
        when(filePlugin.inputStreamUpload(any(), any(), eq("image/webp"))).thenReturn("https://im-bucket.oss.com/test.webp");
        when(imageMapper.insert(any(ImImage.class))).thenReturn(1);

        // 构造最小有效的 WebP 头部：RIFF (4) + size (4) + WEBP (4) + VP8  (4) + 14 dummy bytes = 30 bytes
        byte[] webpBytes = new byte[30];
        System.arraycopy("RIFF".getBytes(StandardCharsets.US_ASCII), 0, webpBytes, 0, 4);
        int size = 30 - 8;
        webpBytes[4] = (byte) (size & 0xFF);
        webpBytes[5] = (byte) ((size >> 8) & 0xFF);
        webpBytes[6] = 0;
        webpBytes[7] = 0;
        System.arraycopy("WEBP".getBytes(StandardCharsets.US_ASCII), 0, webpBytes, 8, 4);
        System.arraycopy("VP8 ".getBytes(StandardCharsets.US_ASCII), 0, webpBytes, 12, 4);

        MockMultipartFile file = new MockMultipartFile("file", "test.webp", "image/webp", webpBytes);
        String url = imImageService.upload(user, "1001", file);

        assertThat(url).isEqualTo("https://im-bucket.oss.com/test.webp");

        ArgumentCaptor<ImImage> captor = ArgumentCaptor.forClass(ImImage.class);
        verify(imageMapper).insert(captor.capture());
        ImImage saved = captor.getValue();
        assertThat(saved.getUploaderId()).isEqualTo("901");
        assertThat(saved.getUploaderRole()).isEqualTo("STORE");
        assertThat(saved.getContentType()).isEqualTo("image/webp");
        assertThat(saved.getObjectKey()).endsWith(".webp");
    }

    @Test
    @DisplayName("上传-数据库保存失败触发OSS对象清理补偿")
    void uploadRollbackOssWhenDbFails() {
        AuthUser user = createMemberUser();
        ImTalk talk = createValidTalk();
        when(imTalkService.getById("1001")).thenReturn(talk);

        OssSetting ossSetting = new OssSetting();
        ossSetting.setAliyunOSSBucketName("im-bucket");
        ossSetting.setAliyunOSSEndPoint("oss-cn-beijing.aliyuncs.com");
        when(storageSettings.getStorage()).thenReturn(ossSetting);

        doReturn(filePlugin).when(imImageService).createStorage(ossSetting);
        when(filePlugin.inputStreamUpload(any(), any(), eq("image/png"))).thenReturn("https://im-bucket.oss.com/test.png");
        when(imageMapper.insert(any(ImImage.class))).thenThrow(new RuntimeException("DB error"));

        MockMultipartFile file = new MockMultipartFile("file", "test.png", "image/png", validPngBytes);
        ServiceException ex = assertThrows(ServiceException.class, () ->
                imImageService.upload(user, "1001", file));

        assertThat(ex.getMessage()).contains("保存客服图片记录失败，请重试");
        verify(filePlugin).deleteFile(anyList());
    }

    @Test
    @DisplayName("校验消息-发送者或接收者不匹配拦截")
    void validateMessageThrowsWhenSenderOrReceiverMismatch() {
        AuthUser user = createMemberUser(); // id: 101
        ImTalk talk = createValidTalk();    // userId1: 101, userId2: 201 (store)
        when(imTalkService.getById("1001")).thenReturn(talk);

        MessageOperation op = new MessageOperation();
        op.setTalkId("1001");
        op.setFrom("101");
        op.setTo("999"); // 不匹配 201
        op.setContext("https://im-bucket.oss.com/test.png");

        ServiceException ex = assertThrows(ServiceException.class, () ->
                imImageService.validateMessage(user, op));
        assertThat(ex.getMessage()).contains("图片消息发送者或接收者与当前会话不符");
    }

    @Test
    @DisplayName("校验消息-图片未上传或外链拦截")
    void validateMessageThrowsWhenImageNotUploaded() {
        AuthUser user = createMemberUser();
        ImTalk talk = createValidTalk();
        when(imTalkService.getById("1001")).thenReturn(talk);

        MessageOperation op = new MessageOperation();
        op.setTalkId("1001");
        op.setFrom("101");
        op.setTo("201");
        op.setContext("https://external.domain.com/evil.png");

        when(imageMapper.selectCount(any())).thenReturn(0L);

        ServiceException ex = assertThrows(ServiceException.class, () ->
                imImageService.validateMessage(user, op));
        assertThat(ex.getMessage()).contains("图片未上传到当前会话");
    }

    @Test
    @DisplayName("校验消息-已上传合规图片校验通过")
    void validateMessageSuccess() {
        AuthUser user = createMemberUser();
        ImTalk talk = createValidTalk();
        when(imTalkService.getById("1001")).thenReturn(talk);

        MessageOperation op = new MessageOperation();
        op.setTalkId("1001");
        op.setFrom("101");
        op.setTo("201");
        op.setContext("https://im-bucket.oss.com/valid.png");

        when(imageMapper.selectCount(any())).thenReturn(1L);

        assertDoesNotThrow(() -> imImageService.validateMessage(user, op));
    }
}
