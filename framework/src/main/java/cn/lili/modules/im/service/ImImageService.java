package cn.lili.modules.im.service;

import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.common.utils.SnowFlake;
import cn.lili.modules.file.plugin.FilePlugin;
import cn.lili.modules.file.plugin.impl.AliFilePlugin;
import cn.lili.modules.im.entity.dos.ImImage;
import cn.lili.modules.im.entity.dos.ImTalk;
import cn.lili.modules.im.entity.vo.MessageOperation;
import cn.lili.modules.im.mapper.ImImageMapper;
import cn.lili.modules.system.entity.dto.OssSetting;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import javax.imageio.ImageIO;
import javax.imageio.ImageReader;
import javax.imageio.stream.ImageInputStream;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.time.LocalDate;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.Objects;

/**
 * 客服图片上传与发送归属校验。桶取自客服设置，店铺取自服务端会话，不接受客户端指定目录或存储位置。
 * OSS 与数据库无法组成同一事务：对象上传后若凭据写入失败，尽力删除本次新对象并报告失败，不重用公共桶。
 * @author Bulbasaur
 * @since 2026-11-09
 */
@Slf4j
@Service
@RequiredArgsConstructor
public class ImImageService {
    private static final long MAX_SIZE = 10L * 1024 * 1024;
    private final ImTalkService imTalkService;
    private final ImImageMapper imageMapper;
    private final ImImageStorageSettings storageSettings;

    /**
     * 上传并保存图片凭据，失败时不返回可发送地址；同一图片地址可在原上传人的原会话中重发。
     * @param user 当前登录账号
     * @param talkId li_im_talk.id
     * @param file 不超过 10MB 的 PNG、JPEG、GIF 或 WebP 图片
     * @return 公共读 HTTPS 图片永久地址
     */
    public String upload(AuthUser user, String talkId, MultipartFile file) {
        ImTalk talk = authorizedTalk(user, talkId);
        if (file == null || file.isEmpty() || file.getSize() > MAX_SIZE) {
            throw new ServiceException("请选择不超过10MB的图片");
        }
        byte[] bytes;
        try (var stream = file.getInputStream()) {
            bytes = stream.readNBytes((int) MAX_SIZE + 1);
        } catch (IOException e) {
            throw new ServiceException("读取图片失败，请重新选择");
        }
        if (bytes.length == 0 || bytes.length > MAX_SIZE) {
            throw new ServiceException("请选择不超过10MB的图片");
        }
        String extension = imageExtension(bytes);
        OssSetting setting = storageSettings.getStorage();
        String storeId = storeId(talk);
        String imageId = SnowFlake.getIdStr();
        String date = LocalDate.now(ZoneId.of("Asia/Shanghai")).format(DateTimeFormatter.ofPattern("yyyy/MM/dd"));
        String key = "im/store/" + storeId + "/talk/" + talkId + "/" + date + "/" + imageId + "." + extension;
        String contentType = "image/" + ("jpg".equals(extension) ? "jpeg" : extension);
        FilePlugin plugin = createStorage(setting);
        String url = plugin.inputStreamUpload(new ByteArrayInputStream(bytes), key, contentType);
        ImImage image = new ImImage();
        image.setId(imageId);
        image.setStoreId(storeId);
        image.setTalkId(talkId);
        image.setUploaderId(user.getId());
        image.setUploaderRole(user.getRole().name());
        image.setBucket(setting.getAliyunOSSBucketName());
        image.setEndpoint(setting.getAliyunOSSEndPoint());
        image.setObjectKey(key);
        image.setUrl(url);
        image.setContentType(contentType);
        image.setFileSize((long) bytes.length);
        image.setCreateTime(new Date());
        try {
            if (imageMapper.insert(image) != 1) throw new ServiceException("保存客服图片记录失败");
        } catch (RuntimeException e) {
            try {
                plugin.deleteFile(List.of(key));
            } catch (RuntimeException cleanupError) {
                log.error("客服图片凭据写入失败，补偿删除对象失败，图片ID：{}", imageId, cleanupError);
            }
            throw new ServiceException("保存客服图片记录失败，请重试");
        }
        return url;
    }

    /**
     * 校验 PICTURE 消息的真实发送者、接收者与上传凭据，拒绝任意外链和跨会话引用。
     * @param user WebSocket 登录凭据
     * @param operation 待发送图片消息，正文是本接口已返回的永久 URL
     */
    public void validateMessage(AuthUser user, MessageOperation operation) {
        ImTalk talk = authorizedTalk(user, operation.getTalkId());
        String self = user.getRole() == UserEnums.STORE ? user.getStoreId() : user.getId();
        String other = Objects.equals(self, talk.getUserId1()) ? talk.getUserId2() : talk.getUserId1();
        if (!Objects.equals(self, operation.getFrom()) || !Objects.equals(other, operation.getTo())) {
            throw new ServiceException("图片消息发送者或接收者与当前会话不符");
        }
        if (operation.getContext() == null || operation.getContext().length() > 1024) {
            throw new ServiceException("无效的客服图片地址");
        }
        Long count = imageMapper.selectCount(new LambdaQueryWrapper<ImImage>()
                .eq(ImImage::getTalkId, talk.getId()).eq(ImImage::getStoreId, storeId(talk))
                .eq(ImImage::getUploaderId, user.getId()).eq(ImImage::getUploaderRole, user.getRole().name())
                .eq(ImImage::getUrl, operation.getContext()));
        if (count == 0) throw new ServiceException("图片未上传到当前会话，请重新选择图片");
    }

    /**
     * 校验当前账号是否是会话中的客户或店铺；用户之间的聊天不属于客服图片存储范围。
     * @param user 已验签、校验登录状态的账号
     * @param talkId 会话 ID，必须为数字以保持对象路径结构
     * @return 校验后的客户与店铺会话
     */
    private ImTalk authorizedTalk(AuthUser user, String talkId) {
        if (user == null || user.getId() == null || talkId == null || !talkId.matches("[0-9]+")) {
            throw new ServiceException("登录失效或客服会话无效");
        }
        ImTalk talk = imTalkService.getById(talkId);
        if (talk == null || Boolean.TRUE.equals(talk.getDeleteFlag())) throw new ServiceException("客服会话不存在");
        String store = storeId(talk);
        String member = Boolean.TRUE.equals(talk.getStoreFlag1()) ? talk.getUserId2() : talk.getUserId1();
        boolean permitted = user.getRole() == UserEnums.MEMBER && Objects.equals(user.getId(), member)
                || user.getRole() == UserEnums.STORE && Objects.equals(user.getStoreId(), store);
        if (!permitted) throw new ServiceException("无权访问该客服会话");
        return talk;
    }

    /**
     * 从会话双方的店铺标识推导唯一店铺，拒绝缺失标识、双店铺及非法路径 ID。
     * @param talk 客服会话
     * @return li_store.id
     */
    private String storeId(ImTalk talk) {
        boolean first = Boolean.TRUE.equals(talk.getStoreFlag1());
        if (first == Boolean.TRUE.equals(talk.getStoreFlag2())) throw new ServiceException("会话没有唯一所属店铺");
        String store = first ? talk.getUserId1() : talk.getUserId2();
        if (store == null || !store.matches("[0-9]+")) throw new ServiceException("会话店铺无效");
        return store;
    }

    /**
     * 复用现有阿里云上传插件，仅传入客服专用配置，不读取全局桶或修改桶权限。
     * @param setting 经校验的客服 OSS 设置
     * @return 文件操作插件
     */
    protected FilePlugin createStorage(OssSetting setting) {
        return new AliFilePlugin(setting);
    }

    /**
     * 根据内容识别格式，忽略客户端 MIME 与文件名；常规格式读取图片头尺寸，WebP 校验容器及图像块。
     * @param bytes 原始文件内容
     * @return 服务端确定的扩展名
     */
    private String imageExtension(byte[] bytes) {
        if (bytes.length >= 30 && "RIFF".equals(new String(bytes, 0, 4, StandardCharsets.US_ASCII))
                && "WEBP".equals(new String(bytes, 8, 4, StandardCharsets.US_ASCII))) {
            long size = (bytes[4] & 255L) | (bytes[5] & 255L) << 8 | (bytes[6] & 255L) << 16 | (bytes[7] & 255L) << 24;
            String chunk = new String(bytes, 12, 4, StandardCharsets.US_ASCII);
            if (size == bytes.length - 8L && List.of("VP8 ", "VP8L", "VP8X").contains(chunk)) return "webp";
        }
        try (ImageInputStream input = ImageIO.createImageInputStream(new ByteArrayInputStream(bytes))) {
            Iterator<ImageReader> readers = ImageIO.getImageReaders(input);
            if (readers.hasNext()) {
                ImageReader reader = readers.next();
                try {
                    reader.setInput(input);
                    String format = reader.getFormatName().toLowerCase(java.util.Locale.ROOT);
                    if (reader.getWidth(0) > 0 && reader.getHeight(0) > 0 && List.of("png", "jpeg", "gif").contains(format)) {
                        return "jpeg".equals(format) ? "jpg" : format;
                    }
                } finally {
                    reader.dispose();
                }
            }
        } catch (IOException | RuntimeException e) {
            throw new ServiceException("图片内容损坏，请重新选择");
        }
        throw new ServiceException("仅支持有效的 PNG、JPG、GIF 或 WebP 图片");
    }
}
